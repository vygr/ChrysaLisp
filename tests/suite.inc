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

(import "lib/files/files.inc")
(import "lib/task/pipe.inc")
(import "lib/task/cmd.inc")

(defq *test_passes* 0 *test_failures* 0 *test_skips* 0
	*test_verbose* :nil *test_frames* :nil *test_module* ""
	;scripts under tests/ that are named test_ but are not suite modules
	+test_excludes ''("tests/net/test_cluster.lisp")
	;the last line a batch's task prints is its counts, and how long to
	;give a task before a retry
	+test_counts "=== " +test_retry 30000000
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

(defun test-collect (cmd out)
	; (test-collect cmd out)
	;print what a batch of modules printed, and add its counts to ours. No
	;counts line means the task did not get to the end of the batch.
	(defq lines (split (ifn out "") "\n")
		counts (if (nempty? lines) (last lines) ""))
	(cond
		((starts-with +test_counts counts)
			(each (const print) (slice lines 0 -2))
			(bind '(passes failures skips) (map (const str-as-num)
				(split (slice counts (length +test_counts) -1) " ")))
			(setq *test_passes* (+ *test_passes* passes)
				*test_failures* (+ *test_failures* failures)
				*test_skips* (+ *test_skips* skips)))
		(:t (each (const print) lines)
			(setq *test_module* cmd)
			(test-fail "batch" "to run to the end" "no result"))))

(defun run-suite (modules &optional verbose frames jobs child)
	; (run-suite modules [verbose frames jobs child]) -> :t | :nil
	;run the modules, at most jobs of them to a task. If they all fit in
	;one batch they run here, one after another. A child is the task of a
	;batch, it ends with a line of counts in place of the summary.
	(setd jobs 1)
	(setq *test_passes* 0 *test_failures* 0 *test_skips* 0
		*test_verbose* verbose *test_frames* frames)
	;services the modules rely on
	(unless child (open-child "service/lock/app.lisp" +kn_call_run))
	;a module in tests/solo/ changes the network, adds a node say, which
	;would upset the others, so those run here, one at a time, at the end
	(defq solo (filter (# (starts-with "tests/solo/" %0)) modules)
		others (filter (# (not (starts-with "tests/solo/" %0))) modules))
	(cond
		((or child (<= (length modules) jobs))
			(each (# (test-run-module %0)) others))
		(:t ;farm out the batches, show the results in module order
			(defq opts (cat " -c -j " (str jobs) (if verbose " -v" "") (if frames " -f" ""))
				cmds (map (# (cat "tests" opts " " (join %0 " ")))
					(partition others jobs))
				outs (Fmap 31))
			(each (lambda ((cmd out)) (. outs :insert cmd out))
				(pipe-farm (cat cmds) +test_retry))
			(each (# (test-collect %0 (. outs :find %0))) cmds)))
	(each (# (test-run-module %0)) solo)
	(if frames (setq *stack_frame* :nil))
	(cond
		(child (print +test_counts *test_passes* " " *test_failures* " " *test_skips*))
		(:t (if *test_verbose* (print))
			(print "=== Test Summary ===")
			(print "Modules: " (length modules))
			(print "Passed: " *test_passes*)
			(print "Failed: " *test_failures*)
			(print "Skipped: " *test_skips*
				(if (or *test_checked* (= *test_skips* 0)) ""
					", release build, no error checks to test"))
			(print "RESULT: " (if (= *test_failures* 0) "SUCCESS" "FAILURE"))))
	(= *test_failures* 0))
