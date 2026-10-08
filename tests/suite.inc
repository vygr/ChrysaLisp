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
	;the last line a batch's task prints is its counts, and how long to
	;give a task before a retry. Before it, a line for each module
	+test_counts "=== " +test_module_counts "=#= " +test_retry 30000000
	;what is known of each file, (age hash names), the counts of each
	;module that was run, and the commands there are
	*test_files* (Fmap 101) *test_done* (Fmap 31) *test_cmds* :nil *test_child* :nil
	;an error checked build throws on bad arguments, a release build has no
	;checks. An extra argument is ignored by release, so is safe to try.
	*test_checked* (catch (progn (length "abc" 1) :nil) :t))

;;;;;;;;;;;
; reporting
;;;;;;;;;;;

(defun test-pass (name)
	(++ *test_passes*)
	(if *test_verbose* (print "[PASS] " name)))

(defun test-fail (name expected got)
	(++ *test_failures*)
	(print "[FAIL] " *test_module* ": " name " | Expected: " expected " | Got: " got))

(defun test-skip (name why)
	; (test-skip name why)
	;a test that can't run here, shown in verbose mode, counted in the summary
	(++ *test_skips*)
	(if *test_verbose* (print "[SKIP] " name ", " why)))

(defun report-header (section_name)
	(when *test_verbose*
		(print)
		(print "=== Testing: " section_name " ===")))

(defun test-equal? (a b)
	; (test-equal? a b) -> :t | :nil
	;lists compare by content, to any depth, all else by eql. A list also
	;matches a nums or other vector with the same elements, as an expected
	;value can only be written as a list when it is nested.
	(if (and (array? a) (array? b) (or (list? a) (list? b)))
		(and (= (length a) (length b)) (every test-equal? a b))
		(eql a b)))

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

(defun test-check (name expected got)
	(if (test-equal? expected got)
		(test-pass name)
		(test-fail name expected got)))

;;;;;;;;;;;;
; assertions
;;;;;;;;;;;;

(defmacro assert-eq (name expected form)
	; (assert-eq name expected form)
	;the form must give a result that is eql to expected
	(defq res (gensym) exp (gensym))
	`(progn
		(defq ,res ,form ,exp ,expected)
		(if (eql ,exp ,res)
			(test-pass ,name)
			(test-fail ,name ,exp ,res))))

(defmacro assert-true (name form)
	; (assert-true name form)
	`(assert-eq ,name :t (if ,form :t :nil)))

(defmacro assert-list-eq (name expected form)
	; (assert-list-eq name expected form)
	;the form must print the same as expected, so a list matches a nums
	(defq res (gensym) exp (gensym))
	`(progn
		(defq ,res ,form ,exp ,expected)
		(if (eql (str ,exp) (str ,res))
			(test-pass ,name)
			(test-fail ,name ,exp ,res))))

(defmacro assert-error (name form)
	; (assert-error name form)
	;the form must throw an error. Only an error checked build can tell,
	;on a release build the form is not run and the test is skipped.
	`(if *test_checked*
		(if (catch (progn ,form :nil) :t)
			(test-pass ,name)
			(test-fail ,name "an error" "no error"))
		(test-skip ,name "needs an error checked build")))

(defmacro test-cases (&rest cases)
	; (test-cases form expected form expected ...)
	;a table of forms and the result each must give, lists compared by
	;content. Each case is named by the text of its form.
	(cat '(progn) (map (lambda ((form expected))
		(list 'test-check (str form) expected form)) (partition cases 2))))

;;;;;;;;;;;;;;;;;;;;;;
; the cache of results
;;;;;;;;;;;;;;;;;;;;;;

(defun test-cache-path ()
	;the cache is with the objects of the CPU and ABI that is running, so
	;each machine, and the emulator, has its own
	(cat (load-path) "tests/cache"))

(defun test-hex (bytes)
	;the first 16 bytes, as hex
	(apply (const cat) (map (# (byte-to-hex-str (code %0))) (slice bytes 0 16))))

(defun test-file-scan (file text)
	; (test-file-scan file text) -> paths
	;the files a Lisp file names. What it imports or includes. Any word
	;of a line with a string on it that is the path of a file. And the
	;command a string starts with, or has after a |. It can name too
	;much, which costs a run that was not needed, and that is the safe
	;way to be wrong.
	(unless *test_cmds*
		(setq *test_cmds* (map (# (slice %0 4 -6)) (files-all "cmd" '(".lisp")))))
	(defq names (list))
	(each (lambda (line)
		(when (or (find (ascii-char 34) line) (find "{" line))
			(defq pieces (split line (const (cat (ascii-char 34) "{}"))))
			(each (lambda (piece)
				(cond
					((and (> (!) 0)
							(or (ends-with "(import " (defq before (elem-get pieces (dec (!)))))
								(ends-with "(include " before)))
						(defq name (catch (path-to-absolute piece file) :nil))
						(if (and name (/= (age name) 0) (not (find name names))) (push names name)))
					(:t (each (lambda (part)
							(when (nempty? (defq words (split part " ")))
								(if (find (first words) *test_cmds*)
									(unless (find (defq name (cat "cmd/" (first words) ".lisp")) names)
										(push names name)))
								(each (lambda (word)
									(if (and (find "/" word) (not (find word names))
											(/= (age word) 0) (nql word file))
										(push names word))) words)))
						(split piece "|")))))
				pieces)))
		(split text (ascii-char 10)))
	names)

(defun test-file-info (file)
	; (test-file-info file) -> (age hash names)
	;what is known of a file. It is read only if it has changed since it
	;was last looked at
	(defq info (. *test_files* :find file) now (age file))
	(unless (and info (= (first info) now))
		(defq text (ifn (load file) ""))
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

(defun test-base ()
	; (test-base) -> str
	;what every module stands on, the boot image, the host programs, the
	;Lisp every task starts with, and the suite itself with all that it
	;imports, which a module can call
	(test-hash-of (cat
		(list (cat (load-path) "sys/boot_image"))
		(sort (files-all (slice (load-path) 0 -2) '("main_gui" "main_tui" "main_gui.exe" "main_tui.exe")))
		(test-stands-on (list "class/lisp/root.inc" "class/lisp/task.inc" "tests/suite.inc" "cmd/tests.lisp")))))

(defun test-key (module base)
	; (test-key module base) -> str
	;the hash of all a module stands on
	(test-hex (sha256 (cat base (test-hash-of (test-stands-on (list module)))))))

(defun test-cache-load ()
	; (test-cache-load) -> fmap
	;what was kept, of the files to *test_files*, and of the modules as a
	;map of (key passes failures skips)
	(defq kept (Fmap 31) tab (ascii-char 9))
	(each (lambda (line)
		(defq fields (split line tab))
		(cond
			((and (eql (first fields) "F") (>= (length fields) 4))
				(. *test_files* :insert (second fields) (list (str-as-num (third fields))
					(elem-get fields 3) (if (> (length fields) 4) (split (elem-get fields 4) " ") (list)))))
			((and (eql (first fields) "M") (= (length fields) 6))
				(. kept :insert (second fields) (cat (list (third fields))
					(map (const str-as-num) (slice fields 3 6)))))))
		(split (ifn (load (test-cache-path)) "") (ascii-char 10)))
	kept)

(defun test-cache-save (kept)
	; (test-cache-save kept)
	(defq lines (list) tab (ascii-char 9))
	;a file or a module that is gone is not kept
	(. *test_files* :each (lambda (file (mtime hash names))
		(if (/= mtime 0)
			(push lines (cat "F" tab file tab (str mtime) tab hash tab (join names " "))))))
	(. kept :each (lambda (module (key passes failures skips))
		(if (/= (age module) 0)
			(push lines (cat "M" tab module tab key tab (str passes) tab (str failures) tab (str skips))))))
	(save (join (sort lines) (ascii-char 10)) (test-cache-path)))

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

(defun test-run-module (file)
	(defq *test_module* (slice file 6 -6))
	(env-push)
	;with frames on, functions the module defines or imports record their
	;calls, so an error says what was running, at some cost in speed
	(if *test_frames* (import "lib/debug/frames.inc"))
	(catch (import file)
		(progn (test-fail "module" "to run to the end" _) :t))
	(env-pop))

(defun test-run-counted (file)
	;run a module, and note what it alone counted. The task of a batch
	;prints that, for the task that farmed it out to note
	(defq passes *test_passes* failures *test_failures* skips *test_skips*)
	(test-run-module file)
	(setq passes (- *test_passes* passes) failures (- *test_failures* failures)
		skips (- *test_skips* skips))
	(if *test_child*
		(print +test_module_counts file " " passes " " failures " " skips)
		(. *test_done* :insert file (list passes failures skips))))

(defun test-collect (cmd out)
	; (test-collect cmd out)
	;print what a batch of modules printed, and add its counts to ours. No
	;counts line means the task did not get to the end of the batch.
	(defq lines (split (ifn out "") "\n")
		counts (if (nempty? lines) (last lines) ""))
	(cond
		((starts-with +test_counts counts)
			(each (# (cond
				((starts-with +test_module_counts %0)
					(defq fields (split (slice %0 (length +test_module_counts) -1) " "))
					(. *test_done* :insert (first fields) (map (const str-as-num) (rest fields))))
				((print %0)))) (slice lines 0 -2))
			(bind '(passes failures skips) (map (const str-as-num)
				(split (slice counts (length +test_counts) -1) " ")))
			(setq *test_passes* (+ *test_passes* passes)
				*test_failures* (+ *test_failures* failures)
				*test_skips* (+ *test_skips* skips)))
		(:t (each (const print) lines)
			(setq *test_module* cmd)
			(test-fail "batch" "to run to the end" "no result"))))

(defun run-suite (modules &optional verbose frames jobs child all stale)
	; (run-suite modules [verbose frames jobs child all stale]) -> :t | :nil
	;run the modules, at most jobs of them to a task. If they all fit in
	;one batch they run here, one after another. A child is the task of a
	;batch, it ends with a line of counts in place of the summary.
	;A module whose result is in the cache, and still stands, is not run,
	;unless all are asked for. With stale the modules that would be run
	;are listed and none is.
	(setd jobs 1)
	(setq *test_passes* 0 *test_failures* 0 *test_skips* 0
		*test_verbose* verbose *test_frames* frames *test_child* child)
	(defq run modules kept :nil keys (Fmap 31))
	(unless child
		(setq kept (test-cache-load))
		(defq base (test-base))
		(setq run (filter (lambda (module)
			(defq key (test-key module base) was (. kept :find module))
			(. keys :insert module key)
			(cond
				((and was (not all) (eql (first was) key))
					(bind '(& passes failures skips) was)
					(setq *test_passes* (+ *test_passes* passes)
						*test_failures* (+ *test_failures* failures)
						*test_skips* (+ *test_skips* skips))
					:nil)
				(:t))) modules)))
	(when stale
		(each (const print) run)
		(test-cache-save kept)
		(setq run (list)))
	;services the modules rely on
	(unless (or child (empty? run)) (open-child "service/lock/app.lisp" +kn_call_run))
	;a module in tests/solo/ changes the network, adds a node say, which
	;would upset the others, so those run here, one at a time, at the end
	(defq solo (filter (# (starts-with "tests/solo/" %0)) run)
		others (filter (# (not (starts-with "tests/solo/" %0))) run))
	(cond
		((or child (<= (length run) jobs))
			(each (const test-run-counted) others))
		(:t ;farm out the batches, show the results in module order
			(defq opts (cat " -c -j " (str jobs) (if verbose " -v" "") (if frames " -f" ""))
				cmds (map (# (cat "tests" opts " " (join %0 " ")))
					(partition others jobs))
				outs (Fmap 31))
			(each (lambda ((cmd out)) (. outs :insert cmd out))
				(pipe-farm (cat cmds) +test_retry))
			(each (# (test-collect %0 (. outs :find %0))) cmds)))
	(each (const test-run-counted) solo)
	;what ran to its end with nothing failed is kept. What failed is
	;not, so it is run again the next time, whatever has changed
	(unless (or child stale)
		(each (lambda (module)
			(defq counts (. *test_done* :find module))
			(if (and counts (= (second counts) 0))
				(. kept :insert module (cat (list (. keys :find module)) counts))
				(. kept :erase module))) run)
		(test-cache-save kept))
	(if frames (setq *stack_frame* :nil))
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
			(print "Skipped: " *test_skips*
				(if (or *test_checked* (= *test_skips* 0)) ""
					", release build, no error checks to test"))
			(print "RESULT: " (if (= *test_failures* 0) "SUCCESS" "FAILURE"))))
	(= *test_failures* 0))
