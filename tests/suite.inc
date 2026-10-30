;;;;;;;;;;;;;;;;;;;;;
; unit test framework
;;;;;;;;;;;;;;;;;;;;;

; Run from the TUI with: tests
; Or host shell: echo "tests" | ./run_tui.sh -f
; See 'tests -h' for the options.

; A test module is any tests/<category>/test_<name>.lisp file. It is found
; automatically, and runs as top level code in an environment of its own, so
; nothing it defines is seen by the next module.

; The modules run in parallel, in batches of -j modules to a task, farmed
; out over the nodes. The default is one. A module can so make no use of
; what another leaves behind, and must name any file or service it makes
; so as not to clash. Tests that do depend on each other, as the lock
; service tests do through its history, must be in the one module.
; A -j larger than the number of modules runs them all in the one task.

; The results are a cache. What a module gave is kept, with a hash of all
; it stands on, and while that hash is the same the module is not run
; again, its counts are taken from the cache. What it stands on is the
; module, every file it imports and they import, every file any of them
; names, a shader or an app say, every command they run, and under them all
; the boot image, the host programs and the Lisp every task starts with. So
; a change runs again just what stands on it, a change to a library its
; own tests, a change to the boot image all of them. 'tests -a' runs them
; all whatever the cache has, which is what a release is tested with.

(import "lib/files/files.inc")
(import "lib/task/pipe.inc")
(import "lib/task/cmd.inc")
(import "lib/crypto/sha256.inc")

(defq *test_passes* 0 *test_failures* 0 *test_skips* 0
	*test_verbose* :nil *test_frames* :nil *test_module* ""
	;scripts under tests/ that are named test_ but are not suite modules
	+test_excludes ''("tests/net/test_cluster.lisp")
	;the last line a batch's task prints is its counts, and how long a
	;task may say nothing before a retry. Before it, a line for each
	;module. The retry is for a node that has gone: a batch that is slow,
	;all of the suite one module after another on an emulated CPU, makes
	;checks as it goes, and each one starts its time again. Run again
	;beside itself it would be slower, and after three the farm stops
	;and every batch still out has no result
	+test_counts "=== " +test_module_counts "=#= " +test_retry 300000000
	;the line of a module that looks at every source file of the tree
	+test_whole_tree ";stands on the whole tree"
	;with -p a run says how it goes, a line each, for a progress bar or
	;whatever reads it as it comes: what is to run and why, what was kept,
	;each module as it starts and as it is done, and when it is over
	+test_progress "=p= " *test_progress* :nil *test_t0* 0 *test_ran* 0 *test_to_run* 0
	;the batches, and how long each module took the last time it ran, ms,
	;and of each batch out, when it went, how long it took the last time
	;and when it is next said to be late
	*test_batches* :nil *test_last* (Fmap 31) *test_out* (Fmap 31)
	;each check as it is made, mailed to the task that farmed its module,
	;-r: the module, which check it is of it, where and its name. That
	;task keeps the last of each module and says it, so a module that hangs
	;is seen at the check it last made. Where it is, file:line, is the
	;reader's as the assertion's macro is expanded, (repl-info): the file it
	;is in, the module or one it imports, and the line the top level form
	;it is in ends on, its own when it is at the top level
	*test_report* :nil *test_file* "" *test_seq* 0 *test_checks* :nil *test_at* (Fmap 31)
	;a batch is late past three times its time the last time and 10 seconds,
	;or 30 seconds if it has none, and is said to be so again each time it is
	;as late again. A full run has the machine busy, and a module takes some
	;times its time alone, not more
	+test_late_times 3 +test_late_ms 10000 +test_late_unknown 30000
	;what is known of each file, (age hash names), the counts of each
	;module that was run, and the commands there are
	*test_files* (Fmap 101) *test_done* (Fmap 31) *test_cmds* :nil *test_child* :nil)

;;;;;;;;;;;
; reporting
;;;;;;;;;;;

(defun test-where ()
	; (test-where) -> str
	;where the reader is, file:line, for an assertion's macro to keep
	(bind '(file line) (repl-info))
	(cat file ":" (str line)))

(defun test-told (name where)
	;a check is made, said to the task that farmed the module if there is
	;one, with where it is, file:line, if it is known
	(++ *test_seq*)
	(if *test_report* (mail-send *test_report*
		(cat *test_file* " " (str *test_seq*) " " (ifn where "-") " " name))))

(defun test-note (text)
	; (test-note text)
	;what a module waits on, said to the task that farmed it as a check is,
	;and not counted: a wait that does not end says what it waits for. It
	;has the number of the check before it, and takes that check's place
	(if *test_report* (mail-send *test_report* (cat *test_file* " " (str *test_seq*) " - " text)))
	(if *test_verbose* (print "[NOTE] " text)))

(defun test-pass (name &optional where)
	;a module gives way to the others of its node between one thing held
	;to and the next. One that works for a second with never a mail is a
	;second that every task on its node is late by, (test-watch)
	(task-slice)
	(++ *test_passes*)
	(test-told name where)
	(if *test_verbose* (print "[PASS] " name)))

(defun test-fail (name expected got &optional where)
	(++ *test_failures*)
	(test-told name where)
	(print "[FAIL] " *test_module* ": " name " | Expected: " expected " | Got: " got))

(defun test-skip (name why &optional where)
	; (test-skip name why [where])
	;a test that can't run here, shown in verbose mode, counted in the summary
	(++ *test_skips*)
	(test-told name where)
	(if *test_verbose* (print "[SKIP] " name ", " why)))

(defun report-header (section_name)
	(when *test_verbose*
		(print)
		(print "=== Testing: " section_name " ===")))

(defun test-equal? (a b)
	; (test-equal? a b) -> :t | :nil
	;lists compare by content, to any depth, all else by eql. A list also
	;matches a nums or other vector with the same elements, as an expected
	;value can only be written as a list when it is nested. The pairs
	;still to compare are a list, this does not call itself
	(defq pairs (list a b) same :t)
	(while (and same (nempty? pairs))
		(defq b (pop pairs) a (pop pairs))
		(cond
			((and (array? a) (array? b) (or (list? a) (list? b)))
				(if (= (length a) (length b))
					(each (# (push pairs %0 %1)) a b)
					(setq same :nil)))
			((eql a b))
			((setq same :nil))))
	same)

(defun equal? (a b)
	; (equal? a b) -> :t | :nil
	;loose equality, do they print the same
	(eql (str a) (str b)))

(defun test-output (code)
	; (test-output code) -> str
	;what a snippet of Lisp code prints, run in a task of its own so that
	;nothing reaches the terminal. The code is a command line, so use {}
	;for its strings, and it can not hold a | or a !, they split a pipe.
	(defq out (list))
	(pipe-run (cat "lisp -r " code) (# (push out %0)))
	(join out ""))

(defun test-check (name expected got &optional where)
	(if (test-equal? expected got)
		(test-pass name where)
		(test-fail name expected got where)))

;;;;;;;;;;;;
; assertions
;;;;;;;;;;;;

(defmacro assert-eq (name expected form)
	; (assert-eq name expected form)
	;the form must give a result that is eql to expected
	(defq res (gensym) exp (gensym) where (test-where))
	`(progn
		(defq ,res ,form ,exp ,expected)
		(if (eql ,exp ,res)
			(test-pass ,name ,where)
			(test-fail ,name ,exp ,res ,where))))

(defmacro assert-true (name form)
	; (assert-true name form)
	`(assert-eq ,name :t (if ,form :t :nil)))

(defmacro assert-list-eq (name expected form)
	; (assert-list-eq name expected form)
	;the form must print the same as expected, so a list matches a nums
	(defq res (gensym) exp (gensym) where (test-where))
	`(progn
		(defq ,res ,form ,exp ,expected)
		(if (eql (str ,exp) (str ,res))
			(test-pass ,name ,where)
			(test-fail ,name ,exp ,res ,where))))

(defmacro assert-error (name form)
	; (assert-error name form)
	;the form must throw an error
	(defq where (test-where))
	`(if (catch (progn ,form :nil) :t)
		(test-pass ,name ,where)
		(test-fail ,name "an error" "no error" ,where)))

(defmacro test-cases (&rest cases)
	; (test-cases form expected form expected ...)
	;a table of forms and the result each must give, lists compared by
	;content. Each case is named by the text of its form.
	(defq where (test-where))
	(cat '(progn) (map (lambda ((form expected))
		(list 'test-check (str form) expected form where)) (partition cases 2))))

;;;;;;;;;;;;;;;;;;;;;;
; the cache of results
;;;;;;;;;;;;;;;;;;;;;;

(defun test-cache-path ()
	;the cache is with the objects of the CPU and ABI that is running, so
	;each machine, and the emulator, has its own
	(cat (load-path) "tests/cache"))

(defun test-hex (bytes)
	;the first 16 bytes, as hex
	(apply (const cat) (map! (# (byte-to-hex-str (code %0))) (list bytes) 0 16)))

(defun test-vp-of (folder)
	; (test-vp-of folder) -> paths
	;the .vp files of a folder, not of those in it
	(defq out (list))
	(and (nempty? folder) (/= (age folder) 0)
		(each! (lambda (name type)
				(and (eql type "f") (ends-with ".vp" name) (push out (cat folder "/" name))))
			(unzip (split (pii-dirlist folder) ",") 2)))
	out)

(defun test-def-func? (line fname)
	; (test-def-func? line fname) -> :t | :nil
	;is this the line a native function of that name starts on
	(and (starts-with "(def-func " line) (ends-with (cat "/" fname "\q" "))") line)))

(defun test-natives (file line)
	; (test-natives file line) -> paths
	;the native code a line with an ffi on it calls, each function named
	;as file#name: the .vp file of the file's own folder, or of the folder a
	;path on the line is in, that makes a function of the name the path
	;ends with. Native code that is not the core's, a library's or an
	;app's, is in the boot image, but only what calls a function of it
	;stands on it, and on that function, not the rest of its file
	(defq out (list))
	(each (lambda (piece)
			(each (lambda (word)
					(when (and (find "/" word) (= (age word) 0))
						;(rfind) gives the place after the slash
						(defq at (ifn (rfind "/" word) 1) fname (slice word at -1)
							folder (if (starts-with "./" word)
								(slice file 0 (max 0 (dec (ifn (rfind "/" file) 1))))
								(slice word 0 (dec at))))
						(each (lambda (vp)
								(and (some (# (test-def-func? %0 fname)) (split (ifn (load vp) "") "\n"))
									(not (find (defq name (cat vp "#" fname)) out))
									(push out name)))
							(test-vp-of folder))))
				(split piece " ()")))
		(split line "\q{}"))
	out)

(defun test-native-text (vp fname)
	; (test-native-text vp fname) -> str
	;what a native function is made of: its file's head, the includes and
	;what is set up before the first function, and its own block
	(defq head (list) block (list) in :nil started :nil)
	(each (lambda (line)
			(cond
				(in (push block line) (if (starts-with "(def-func-end)" line) (setq in :nil)))
				((starts-with "(def-func " line)
					(setq started :t)
					(when (test-def-func? line fname) (setq in :t) (push block line)))
				((not started) (push head line))))
		(split (ifn (load vp) "") "\n"))
	(join (cat head block) "\n"))

(defun test-file-scan (file text)
	; (test-file-scan file text) -> paths
	;the files a Lisp file names. What it imports or includes. Any word
	;of a line with a string on it that is the path of a file: a file, not
	;a folder, nothing of .git/, and not a test module, which no module
	;stands on, (test-module?). The native code of a line with an ffi on
	;it. And the command a string starts with, or has after a |, on a line
	;that runs a command line, (test-cmd-line?): a check named "key one" is
	;prose, and named the key command and all it imports. It can still
	;name too much, which costs a run that was not needed, and that is the
	;safe way to be wrong.
	(unless *test_cmds*
		(setq *test_cmds* (map (# (slice %0 4 -6)) (files-all "cmd" '(".lisp")))))
	(defq names (list))
	(each (lambda (line)
		(if (find "ffi" (split line " ()"))
			(each (# (unless (find %0 names) (push names %0))) (test-natives file line)))
		(when (or (find "\q" line) (find "{" line))
			(defq pieces (split line "\q{}") cmd_line (test-cmd-line? line))
			(each (lambda (piece)
				(cond
					((and (> (!) 0)
							(or (ends-with "(import " (defq before (elem-get pieces (dec (!)))))
								(ends-with "(include " before)))
						(defq name (catch (path-to-absolute piece file) :nil))
						(and name (/= (age name) 0) (not (find name names)) (push names name)))
					(:t (each (lambda (part)
							(when (nempty? (defq words (split part " ")))
								(and cmd_line (find (first words) *test_cmds*)
									(not (find (defq name (cat "cmd/" (first words) ".lisp")) names))
									(push names name))
								(each (lambda (word)
									(and (find "/" word) (not (find word names)) (nql word file)
											(not (starts-with ".git/" word)) (not (test-module? word))
											(defq stat (pii-fstat word)) (not (folder-mode? (third stat)))
										(push names word))) words)))
						(split piece "|")))))
				pieces)))
		(split text "\n"))
	names)

(defun test-cmd-line? (line)
	; (test-cmd-line? line) -> :t | :nil
	;does this line run a command line: a string on it may start with a
	;command, and all the command imports is stood on
	(some (# (found? line %0)) '("(pipe-run " "(pipe-farm " "(rack-run " "(Pipe ")))

(defun test-module? (path)
	; (test-module? path) -> :t | :nil
	;a module of the suite, tests/<category>/test_<name>.lisp, which no
	;module stands on: one that names another is not run again for it.
	;Not the suite's own files, nor the scenes of tests/storage/
	(and (starts-with "tests/" path) (ends-with ".lisp" path)
		(starts-with "test_" (slice path (ifn (rfind "/" path) 0) -1))))

;a file that is named and is longer than this is not read
(defq +test_file_most 16777216
	;the rules of what a file names, (test-file-scan): when they change,
	;what a cache made by others says a file names is not kept
	+test_scan_version "3")

(defun test-file-info (file)
	; (test-file-info file) -> (age hash names)
	;what is known of a file. It is read only if it has changed since it
	;was last looked at
	;a native function, file#name, is its file's age and its own block
	(defq hash_at (find "#" file) real (if hash_at (slice file 0 hash_at) file)
		info (. *test_files* :find file) now (age real))
	(unless (and info (= (first info) now))
		;not all of a file that is very big, a film, a store: it is taken as
		;its time and its length. A test that named this machine's store, 4GB
		;of it, asked for that much memory and the run stopped
		(defq stat (pii-fstat real) size (if stat (second stat) 0)
			text (cond
				(hash_at (test-native-text real (slice file (inc hash_at) -1)))
				((> size +test_file_most) (cat (str now) " " (str size)))
				((ifn (load file) ""))))
		(. *test_files* :insert file (setq info (list now (test-hex (sha256 text))
			(if (or (ends-with ".lisp" file) (ends-with ".inc" file))
				(test-file-scan file text) (list))))))
	info)

(defun test-stands-on (files)
	; (test-stands-on files) -> files
	;the files, and every file they name, and those name
	(defq at 0)
	(while (< at (length files))
		(each (# (unless (find %0 files) (push files %0)))
			(third (test-file-info (elem-get files at))))
		(setq at (inc at)))
	(sort files))

(defun test-hash-of (files)
	;the names and the hashes of files, one after another
	(apply (const cat) (map (# (cat %0 (second (test-file-info %0)))) files)))

(defun test-core ()
	; (test-core) -> paths
	;the native code under everything, the core's: the .vp of sys/, class/,
	;gui/ and service/ and all they include, and what builds it, the
	;assembler, the translators and what makes the boot image. The native
	;code of a library or an app is not here: only what calls it stands on
	;it, (test-natives)
	;only what they include, as the native code is built: a service names
	;the Lisp of a library, and that is its Lisp's to stand on, not the core
	(defq files (cat
		(filter (# (some (lambda (top) (starts-with top %0)) '("sys/" "class/" "gui/" "service/")))
			(map (# (if (starts-with "./" %0) (slice %0 2 -1) %0)) (files-all "." '(".vp"))))
		(map (# (if (starts-with "./" %0) (slice %0 2 -1) %0))
			(cat (files-all "lib/asm" '(".inc" ".lisp")) (files-all "lib/trans" '(".inc" ".lisp")) (files-all "lib/boot" '(".inc" ".lisp")))))
		at 0)
	(while (< at (length files))
		(defq file (elem-get files at))
		(each (lambda (line)
				(when (find "\q" line)
					(defq pieces (split line "\q"))
					(each! (lambda (piece)
							(when (ends-with "(include " (elem-get pieces (dec (!))))
								(defq name (catch (path-to-absolute piece file) :nil))
								(and name (/= (age name) 0) (not (find name files)) (push files name))))
						(list pieces) 1)))
			(split (ifn (load file) "") "\n"))
		(setq at (inc at)))
	(sort files))

(defun test-base ()
	; (test-base) -> str
	;what every module stands on: the core's native code by its source, the
	;host programs, the Lisp every task starts with, and the suite itself
	;with all that it imports, which a module can call. Not the boot image
	;as a whole: a library's native code is in it too, and a change to that
	;is a change to what calls it, not to everything
	(test-hash-of (cat
		(test-core)
		(sort (files-all (slice (load-path) 0 -2) '("chrysalisp" "chrysalisp.exe")))
		(test-stands-on (list "class/lisp/root.inc" "class/lisp/task.inc" "tests/suite.inc" "cmd/tests.lisp")))))

(defun test-tree-files ()
	; (test-tree-files) -> paths
	;every source file of the tree, not what tests leave in tests/scratch
	(filter (# (not (starts-with "tests/scratch/" %0)))
		(map (# (if (starts-with "./" %0) (slice %0 2 -1) %0)) (files-all "." '(".lisp" ".inc" ".vp")))))

(defun test-key (module base)
	; (test-key module base) -> str
	;the hash of all a module stands on. One that looks at every source
	;file, check run over the tree, says so in a line of its own, and every
	;file is what it stands on, so a change to any of them runs it again
	(defq files (test-stands-on (list module)))
	(if (found? (ifn (load module) "") +test_whole_tree) (setq files (cat files (test-tree-files))))
	(test-hex (sha256 (cat base (test-hash-of files)))))

(defun test-cache-load ()
	; (test-cache-load) -> fmap
	;what was kept, of the files to *test_files*, and of the modules as a
	;map of (key passes failures skips [ms]), ms how long it took to run
	(defq kept (Fmap 31) tab "\t"
		lines (split (ifn (load (test-cache-path)) "") "\n")
		scans (some (# (if (starts-with (cat "V" tab) %0) (eql (slice %0 2 -1) +test_scan_version))) lines))
	(each (lambda (line)
		(defq fields (split line tab))
		(cond
			((and scans (eql (first fields) "F") (>= (length fields) 4))
				(. *test_files* :insert (second fields) (list (str-as-num (third fields))
					(elem-get fields 3) (if (> (length fields) 4) (split (elem-get fields 4) " ") (list)))))
			((and (eql (first fields) "M") (<= 6 (length fields) 7))
				;the key, then the three counts and the time, into the one list
				(. kept :insert (second fields)
					(map! (const str-as-num) (list fields) 3 -1 (list (third fields)))))))
		lines)
	kept)

(defun test-cache-save (kept)
	; (test-cache-save kept)
	(defq lines (list (cat "V" "\t" +test_scan_version)) tab "\t")
	;a file or a module that is gone is not kept
	(. *test_files* :each (lambda (file (mtime hash names))
		(if (/= mtime 0)
			(push lines (cat "F" tab file tab (str mtime) tab hash tab (join names " "))))))
	(. kept :each (lambda (module entry)
		(if (/= (age module) 0)
			(push lines (cat "M" tab module tab (join (map (const str) entry) tab))))))
	(save (join (sort lines) "\n") (test-cache-path)))

;;;;;;;;
; runner
;;;;;;;;

(defun test-modules (&optional pattern)
	; (test-modules [pattern]) -> paths
	;all the suite modules, sorted, those with pattern in the path if given
	(sort (filter (# (and
			(starts-with "test_" (slice %0 (rfind "/" %0) -1))
			(not (starts-with "tests/scratch/" %0))
			(not (find %0 +test_excludes))
			(or (not pattern) (found? %0 pattern))))
		(files-all "tests" '(".lisp")))))

;with leaks, a module that leaves more than this held on its node, once
;what a task's end lets go of is let go of, fails, (test-leak-run)
(defq +test_leak_kb 256)

(defun test-run-module (file)
	(setq *test_file* file *test_seq* 0)
	(defq *test_module* (slice file 6 -6))
	(env-push)
	;with frames on, functions the module defines or imports record their
	;calls, so an error says what was running, at some cost in speed
	(if *test_frames* (import "lib/debug/frames.inc"))
	(catch (import file)
		(progn (test-fail "module" "to run to the end" _) :t))
	(env-pop))

(defun test-collect (cmd out)
	; (test-collect cmd out)
	;print what a batch of modules printed, and add its counts to ours. No
	;counts line means the task did not get to the end of the batch.
	(defq lines (split (ifn out "") "\n")
		counts (if (nempty? lines) (last lines) ""))
	(cond
		((starts-with +test_counts counts)
			(each! (# (cond
				((starts-with +test_module_counts %0)
					(defq fields (split (slice %0 (length +test_module_counts) -1) " "))
					(. *test_done* :insert (first fields) (map! (const str-as-num) (list fields) 1)))
				((print %0)))) (list lines) 0 -2)
			(bind '(passes failures skips) (map (const str-as-num)
				(split (slice counts (length +test_counts) -1) " ")))
			(setq *test_passes* (+ *test_passes* passes)
				*test_failures* (+ *test_failures* failures)
				*test_skips* (+ *test_skips* skips)))
		(:t (each (const print) lines)
			(setq *test_module* cmd)
			(test-fail "batch" "to run to the end" "no result"))))

(defun test-held ()
	; (test-held) -> kb
	;what this node holds, once what a task's end lets go of is let go of:
	;the cells in use, not the total less what is free, which moves as a
	;heap takes a block or gives one back
	(/ (elem-get (kernel-stats :t) 5) 1024))

(defun test-leak-run (module)
	;a module run alone, in a task of its own on this node, twice: what it
	;says the first time is counted, and what this node holds before the
	;second and after its task has gone is compared. More held then is a
	;leak, what grows each time it is run; what is made once and kept, the
	;glyphs of a font, is not. Chris: "After each test module we should
	;compare the memory use before and after"
	(defq out (list) cmd (cat "echo ! tests -c " module))
	(pipe-run cmd (# (push out %0)))
	(test-collect cmd (join out ""))
	(defq held (test-held))
	(pipe-run cmd (# %0))
	(defq more (- (test-held) held))
	(when (> more +test_leak_kb)
		(setq *test_module* (slice module 6 -6))
		(test-fail "memory" (cat "no more than " (str +test_leak_kb) "kb held after it") (cat (str more) "kb more held"))))

;;;;;;;;;;
; progress
;;;;;;;;;;

(defun test-say (&rest fields)
	; (test-say field ...)
	;a line of how the run goes, with -p, sent on at once: what reads it
	;is to have it as it happens, not when a buffer is full
	(when *test_progress*
		(print +test_progress (join (map (const str) fields) " "))
		(stream-flush (io-stream 'stdout))))

(defun test-ms ()
	;how long since the run began, as it is said
	(cat (str (/ (- (pii-time) *test_t0*) 1000)) "ms"))

(defun test-done-say (module)
	;a module is done, what it counted and how many of the run are
	(when *test_progress*
		(++ *test_ran*)
		(defq counts (ifn (. *test_done* :find module) (list 0 0 0)))
		(test-say "done" module (first counts) (second counts) (third counts) (test-ms)
			(cat (str *test_ran*) "/" (str *test_to_run*)))))

(defun test-start-say (module)
	;a module goes to be run, and how long it took the last time, if it
	;was ever run here: what reads this can tell from that if it is stuck
	(test-say "start" module (test-ms) (ifn (. *test_last* :find module) "-")))

(defun test-usual (modules)
	; (test-usual modules) -> :nil | ms
	;how long these modules took the last time, :nil if one was not run
	(reduce (# (and %0 (num? (defq ms (. *test_last* :find %1))) (+ %0 ms))) modules 0))

(defun test-checks-read ()
	; (test-checks-read) -> (file ...)
	;the checks the modules out have made since the last second: the
	;latest of each is kept, and said, and the modules that made them
	;given back. Mail is not in order, the highest number is the latest,
	;and a note after a check has the same
	(defq moved (list))
	(while (mail-poll (list *test_checks*))
		(defq msg (mail-read *test_checks*) a (find " " msg) b (find " " msg (inc a)) c (find " " msg (inc b))
			file (slice msg 0 a) n (str-as-num (slice msg (inc a) b)) at (. *test_at* :find file))
		(when (or (not at) (>= n (first at)))
			(. *test_at* :insert file (list n (slice msg (inc b) c) (slice msg (inc c) -1)))
			(unless (find file moved) (push moved file))))
	(each (lambda (file)
			(bind '(n where name) (. *test_at* :find file))
			(test-say "at" file n where name))
		moved)
	moved)

(defun test-at (modules)
	; (test-at modules) -> str
	;where a batch is: its module that is not done, and the check it last
	;made and where, 0 if it has made none
	(defq module (some (# (unless (. *test_done* :find %0) %0)) modules)
		at (if module (. *test_at* :find module)))
	(cond
		((not module) "")
		(at (cat "at " (str (first at)) " " (second at) " " (third at)))
		("at 0")))

(defun test-late-say ()
	;each batch out that has gone past when it was to be done is said to
	;be late, with how long it has been, how long it took the last time,
	;and the check it last made, so a run that hangs says where at once.
	;It is not failed: a module waits for what it tests, and a slow
	;machine is let be slow
	(. *test_out* :each (lambda (cmd out)
		(bind '(t0 usual next) out)
		(when (>= (defq ms (/ (- (pii-time) t0) 1000)) next)
			(elem-set out 2 (* 2 ms))
			(defq modules (ifn (. *test_batches* :find cmd) (list cmd)))
			(test-say "late" (join modules " ")
				(cat (str ms) "ms") (if usual (cat "usual " (str usual) "ms") "usual unknown")
				(test-at modules))))))

(defun test-farm-told (what &optional cmd out)
	;what the farm says of a batch as it goes: it is given to a worker,
	;again if its worker went, or its answer has come, with a line for
	;each module it ran and what that counted, and each second, when what
	;is out and late is said
	(case what
		(:tick
			(defq moved (if *test_checks* (test-checks-read)) heard (list))
			(test-late-say)
			;the batches heard from, so the farm gives one again only when
			;it has said nothing for its time, not when it is slow
			(if moved (. *test_batches* :each (lambda (batch modules)
				(if (some (# (find %0 modules)) moved) (push heard batch)))))
			heard)
		(:start
			(defq modules (ifn (. *test_batches* :find cmd) (list)) usual (test-usual modules))
			(. *test_out* :insert cmd (list (pii-time) usual
				(if usual (+ (* +test_late_times usual) +test_late_ms) +test_late_unknown)))
			(each (const test-start-say) modules))
		(:done (. *test_out* :erase cmd)
			(each (lambda (line)
				(when (starts-with +test_module_counts line)
					(defq fields (split (slice line (length +test_module_counts) -1) " "))
					(. *test_done* :insert (first fields) (map! (const str-as-num) (list fields) 1))
					(test-done-say (first fields))))
			(split (ifn out "") "\n")))))

(defun test-run-counted (file)
	;run a module, and note what it alone counted, and how long it took in
	;ms, of itself, not the wait for a worker. The task of a batch prints
	;that, for the task that farmed it out to note
	(defq passes *test_passes* failures *test_failures* skips *test_skips* t0 (pii-time))
	(test-run-module file)
	(setq passes (- *test_passes* passes) failures (- *test_failures* failures)
		skips (- *test_skips* skips) t0 (/ (- (pii-time) t0) 1000))
	(if *test_child*
		(print +test_module_counts file " " passes " " failures " " skips " " t0)
		(. *test_done* :insert file (list passes failures skips t0))))

(defun test-run-said (file)
	;a module run here, said as it starts and as it is done
	(test-start-say file)
	(test-run-counted file)
	(test-done-say file))

(defun test-watch (nodes)
	; (test-watch nodes) -> (stop back)
	;a task on each of these nodes that sleeps a hundredth of a second
	;and notes how late it wakes, till it is told to stop. A task gives
	;way to the others of its node only when it says so, and one that
	;does not for a second or two is why a test with a deadline fails
	;now and then, on a small machine, with nothing wrong in it
	(defq back (mail-mbox) said (mail-mbox))
	(list (map (lambda (node)
			(open-task (str `(progn (defq back (hex-decode ,(hex-encode back)) worst 0 going :t)
					(while going
						(defq t1 (pii-time))
						(task-sleep 10000)
						(defq late (- (pii-time) t1 10000))
						(if (> late worst) (setq worst late))
						(if (mail-poll (list (task-mbox))) (setq going :nil)))
					(mail-send back (str worst))))
				node +kn_call_pin 0 said)
			(getf (mail-read said) +kn_msg_reply_id))
		nodes) back))

(defun test-watched (watch)
	; (test-watched watch) -> (ms ...)
	;the watchers are stopped, and the longest each was kept waiting
	(bind '(stop back) watch)
	(each (# (mail-send %0 "stop")) stop)
	(sort (map (lambda (&) (/ (str-as-num (ifn (mail-read-timeout back (task-timeout 5)) "0")) 1000)) stop)
		(# (- %1 %0))))

(defun run-suite (modules &optional verbose frames jobs child all stale watch leaks progress report)
	; (run-suite modules [verbose frames jobs child all stale watch leaks progress report]) -> :t | :nil
	;run the modules, at most jobs of them to a task. If they all fit in
	;one batch they run here, one after another. A child is the task of a
	;batch, it ends with a line of counts in place of the summary.
	;A module whose result is in the cache, and still stands, is not run,
	;unless all are asked for. With stale the modules that would be run
	;are listed and none is. With watch, the longest a task was kept
	;waiting on each node while the modules ran is said at the end,
	;(test-watch). With leaks each runs alone, one after another, in a
	;task of its own on this node, and one that leaves the node holding
	;more fails, (test-leak-run). With progress the run says how it goes,
	;(test-say), as it goes, and each check of the modules it farms out.
	;report is the mailbox, in hex, of the task that farmed this one, to
	;mail each check to as it is made
	(setd jobs 1)
	(setq *test_passes* 0 *test_failures* 0 *test_skips* 0
		*test_verbose* verbose *test_frames* frames *test_child* child
		*test_progress* (and progress (not child) (not stale)) *test_t0* (pii-time) *test_ran* 0
		*test_report* (if report (hex-decode report)) *test_checks* (if *test_progress* (mail-mbox)))
	(defq run modules kept :nil keys (Fmap 31) why (Fmap 31))
	(unless child
		(setq kept (test-cache-load))
		(defq base (test-base))
		(setq run (filter (lambda (module)
			(defq key (test-key module base) was (. kept :find module))
			(. keys :insert module key)
			(cond
				;one in tests/release/ is for a release, -a, and no other run
				((and (not all) (starts-with "tests/release/" module))
					(. why :insert module "release") :nil)
				((and was (not all) (eql (first was) key))
					(setq *test_passes* (+ *test_passes* (elem-get was 1))
						*test_failures* (+ *test_failures* (elem-get was 2))
						*test_skips* (+ *test_skips* (elem-get was 3)))
					(. why :insert module "cached")
					:nil)
				;run, with all, or what it stands on has changed, or no
				;result of it is kept, it was never run, or it failed the last time
				((. why :insert module (cond (all "all") ((not was) "unkept")
					((eql (first was) "-") "failed") ("changed")))))) modules)))
	;what is to run, and why, and what is as it was, and how long each
	;took the last time
	(when *test_progress*
		(setq *test_last* (Fmap 31))
		(when kept (. kept :each (# (if (> (length %1) 4) (. *test_last* :insert %0 (elem-get %1 4))))))
		(setq *test_to_run* (length run))
		(test-say "plan" (length run) (length (filter (# (eql (. why :find %0) "cached")) modules)) (length modules))
		(each (lambda (module)
				(defq said (ifn (. why :find module) "all"))
				(cond
					((eql said "cached") (test-say "cached" module))
					((eql said "release") (test-say "skipped" module "release"))
					((test-say "dirty" module said))))
			modules))
	(when stale
		(each (const print) run)
		(test-cache-save kept)
		(setq run (list)))
	;services the modules rely on
	(unless (or child (empty? run)) (open-child "service/lock/app.lisp" +kn_call_run))
	;a module in tests/solo/ changes the network, adds a node say, which
	;would upset the others, so those run here, one at a time, at the end
	(defq solo (filter (# (or (starts-with "tests/solo/" %0) (starts-with "tests/release/" %0))) run)
		others (filter (# (not (or (starts-with "tests/solo/" %0) (starts-with "tests/release/" %0)))) run)
		watching (and watch (not child) (nempty? others) (test-watch (lisp-nodes :t))) waits :nil)
	;batches of modules farmed out, each a task of its own, so this one is
	;free to say as they go which are late; the results in module order
	(defq opts (cat " -c -j " (str jobs) (if verbose " -v" "") (if frames " -f" "")
			(if *test_checks* (cat " -r " (hex-encode *test_checks*)) ""))
		farm (lambda (parts retry)
			(defq cmds (map (# (cat "tests" opts " " (join %0 " "))) parts) outs (Fmap 31))
			;which modules each batch is, for what is said as it goes
			(setq *test_batches* (Fmap 31))
			(each (# (. *test_batches* :insert %0 %1)) cmds parts)
			(each (lambda ((cmd out)) (. outs :insert cmd out))
				(pipe-farm (cat cmds) retry (if *test_progress* (const test-farm-told))))
			(each (# (test-collect %0 (. outs :find %0))) cmds)
			;a batch with no result, its modules are said to be lost
			(each (# (unless (. *test_done* :find %0) (test-say "lost" %0 (test-ms)))) (flatten parts))))
	(cond
		((and leaks (not child))
			(each (const test-leak-run) others))
		(child (each (const test-run-said) others))
		((nempty? others) (farm (partition others jobs) +test_retry)))
	;the watchers go before the modules that count the nodes and what is on them
	(if watching (setq waits (test-watched watching)))
	;those that change the network one at a time, each farmed alone. One is
	;given to a new worker only if it is long past its own time, as one of
	;minutes, a desktop's apps opened over and over, is not to be run again
	;beside itself
	(if child (each (const test-run-said) solo)
		(each (# (farm (list (list %0))
				(max +test_retry (* 20000 (ifn (test-usual (list %0)) 0)))))
			solo))
	;what ran to its end is kept, and how long it took. What failed is kept
	;with no key, so it is run again the next time, whatever has changed:
	;what is kept of it is the time. One that did not run to its end is not
	(unless (or child stale)
		(each (lambda (module)
			(defq counts (. *test_done* :find module))
			(cond
				((not counts) (. kept :erase module))
				((. kept :insert module (cat (list (if (= (second counts) 0) (. keys :find module) "-")) counts)))))
			run)
		(test-cache-save kept))
	(if frames (setq *stack_frame* :nil))
	(test-say "end" (test-ms))
	(cond
		(child (print +test_counts *test_passes* " " *test_failures* " " *test_skips*))
		(stale)
		(:t (if *test_verbose* (print))
			(print "=== Test Summary ===")
			(print "Modules: " (length modules)
				(if (= (length run) (length modules)) ""
					(cat ", " (str (length run)) " run, " (str (- (length modules) (length run)))
						" as they were")))
			(print "Passed: " *test_passes*)
			(print "Failed: " *test_failures*)
			(print "Skipped: " *test_skips*)
			(if waits (print "Waited: " (join (map (const str) waits) " ") " ms, the longest a task was kept from running, each node"))
			(print "RESULT: " (if (= *test_failures* 0) "SUCCESS" "FAILURE"))))
	(= *test_failures* 0))
