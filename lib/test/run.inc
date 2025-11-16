;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Testing Framework - Test Runner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Once the programmer has defined what the tests are these need to
;; be run and the expected effects should be compared with the
;; actual effects. FiveAM provides the function RUN for this
;; purpose, RUN executes a number of tests and collects the results
;; of each individual check.
;;
;; Copyright (c) 2002-2003, Edward Marco Baringer
;; Ported to ChrysaLisp 2025
;; See test.inc for full license details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration
(defq *print-test-names* :t)
(defq *on-error* :continue)  ; :continue, :debug, or :backtrace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency Resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-dependencies (depends-on)
	; (resolve-dependencies deps) -> :t | :nil
	; Check if dependencies are satisfied
	(cond
		((not depends-on) :t)
		((= (typeof depends-on) (const num-type)) :t)  ; Symbol treated as single dep
		((lst? depends-on)
			(defq dep-type (first depends-on))
			(cond
				((eql dep-type 'and)
					(every (lambda (dep)
						(defq test (get-test dep))
						(and test (eql :t (. test :find :status))))
						(rest depends-on)))
				((eql dep-type 'or)
					(some (lambda (dep)
						(defq test (get-test dep))
						(and test (eql :t (. test :find :status))))
						(rest depends-on)))
				((eql dep-type 'not)
					(defq dep (second depends-on))
					(defq test (get-test dep))
					(or (not test) (not (eql :t (. test :find :status)))))
				(:t :t)))
		(:t :t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-test-case (test-name)
	; (run-test-case name) -> :t | :nil
	; Run a single test case
	(defq test (get-test test-name))
	(unless test
		(write *test-dribble* (cat "Unknown test: " (str test-name) (ascii-char 10)))
		(return :nil))

	;; Check if it's a test case
	(unless (eql :test-case (. test :find :type))
		(write *test-dribble* (cat "Not a test case: " (str test-name) (ascii-char 10)))
		(return :nil))

	;; Check dependencies
	(unless (resolve-dependencies (. test :find :depends-on))
		(add-result :skipped test-name "Dependencies not satisfied")
		(. test :insert :status :depends-not-satisfied)
		(return :nil))

	;; Print test name if configured
	(when *print-test-names*
		(write *test-dribble* (cat (ascii-char 10) "Running test " (str test-name) " "))
		(stream-flush *test-dribble*))

	;; Set current test and run
	(setq *current-test* test-name)
	(defq start-results (length *test-results*))

	;; Run the test function with error handling
	(catch
		(let ()
			(funcall (. test :find :test-fn))
			(. test :insert :status :t))
		;; Error handler
		(let ()
			(add-result :error test-name (cat "Unexpected error: " (str _)))
			(. test :insert :status :nil)))

	;; Check if all results from this test passed
	(defq test-results (slice start-results -1 *test-results*))
	(defq all-passed (every test-passed? test-results))

	(unless all-passed
		(. test :insert :status :nil))

	all-passed)

(defun run-test-suite (suite-name)
	; (run-test-suite name) -> :t | :nil
	; Run all tests in a suite
	(when *print-test-names*
		(write *test-dribble* (cat (ascii-char 10) "Running test suite " (str suite-name) (ascii-char 10)))
		(stream-flush *test-dribble*))

	(defq tests (suite-tests suite-name))
	(defq all-passed :t)

	;; Run each test in the suite
	(each (lambda ((name test))
		(unless (run-test-case name)
			(setq all-passed :nil)))
		tests)

	all-passed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Run Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run (test-spec)
	; (run test-spec) -> results
	; Run the test or suite specified by TEST-SPEC
	; TEST-SPEC can be a symbol naming a test or test suite,
	; or a list of test names, or :all for all tests

	;; Clear previous results
	(clear-results)

	;; Reset all test statuses
	(each (lambda ((name test))
		(. test :insert :status :unknown))
		(. *test-registry* :each))

	;; Run based on spec type
	(cond
		((eql test-spec :all)
			;; Run all tests
			(each (lambda ((name test))
				(when (eql :test-case (. test :find :type))
					(run-test-case name)))
				(. *test-registry* :each)))

		((lst? test-spec)
			;; Run list of tests
			(each run-test-case test-spec))

		(:t
			;; Run single test or suite
			(defq test (get-test test-spec))
			(when test
				(if (eql :test-suite (. test :find :type))
					(run-test-suite test-spec)
					(run-test-case test-spec)))))

	;; Return results
	*test-results*)

(defun run! (test-spec)
	; (run! test-spec) -> :t | :nil
	; Run tests and explain results
	; Returns :t if all tests passed, :nil otherwise
	(defq results (run test-spec))
	(explain results))

(defun run-all-tests ()
	; (run-all-tests) -> :t | :nil
	; Run all registered tests and explain results
	(run! :all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Results Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun results-status (results)
	; (results-status results) -> (:t/:nil failed-list skipped-list)
	; Analyze results and return status
	(defq failed (list) skipped (list))

	(each (lambda (result)
		(cond
			((test-failed? result) (push failed result))
			((test-skipped? result) (push skipped result))))
		results)

	(list (= 0 (length failed)) failed skipped))
