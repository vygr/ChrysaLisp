;;;;;;;;;;;;;;;;;;;;;
; unit test framework
;;;;;;;;;;;;;;;;;;;;;

; Run from the TUI with: tests
; Or host shell: echo "tests" | ./run_tui.sh -f
; See 'tests -h' for the options.

; A test module is any tests/<category>/test_<name>.lisp file. It is found
; automatically, and runs as top level code in an environment of its own, so
; nothing it defines is seen by the next module.

(import "lib/files/files.inc")

(defq *test_passes* 0 *test_failures* 0 *test_skips* 0
	*test_verbose* :nil *test_module* ""
	;scripts under tests/ that are named test_ but are not suite modules
	+test_excludes ''("tests/net/test_cluster.lisp")
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
	;lists compare by content, to any depth, all else by eql
	(if (and (list? a) (list? b))
		(and (= (length a) (length b)) (every test-equal? a b))
		(eql a b)))

(defun equal? (a b)
	; (equal? a b) -> :t | :nil
	;loose equality, do they print the same
	(eql (str a) (str b)))

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
	(catch (import file)
		(progn (test-fail "module" "to run to the end" _) :t))
	(env-pop))

(defun run-suite (&optional pattern verbose)
	; (run-suite [pattern verbose]) -> :t | :nil
	(setq *test_passes* 0 *test_failures* 0 *test_skips* 0 *test_verbose* verbose)
	;services the modules rely on
	(open-child "service/lock/app.lisp" +kn_call_run)
	(defq modules (test-modules pattern))
	(each (# (test-run-module %0)) modules)
	(if *test_verbose* (print))
	(print "=== Test Summary ===")
	(print "Modules: " (length modules))
	(print "Passed: " *test_passes*)
	(print "Failed: " *test_failures*)
	(print "Skipped: " *test_skips*
		(if (or *test_checked* (= *test_skips* 0)) ""
			", release build, no error checks to test"))
	(print "RESULT: " (if (= *test_failures* 0) "SUCCESS" "FAILURE"))
	(= *test_failures* 0))
