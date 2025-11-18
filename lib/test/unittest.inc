;; ChrysaLisp Unit Testing Framework
;; Provides macros and functions for writing and running unit tests

(defq
	*test-suite-name* nil
	*test-name* nil
	*test-passed* 0
	*test-failed* 0
	*test-errors* '()
	*test-verbose* t)

(defun test-reset ()
	; Reset test counters
	(setq *test-passed* 0
		*test-failed* 0
		*test-errors* '()))

(defun test-report ()
	; Print test results summary
	(print "")
	(print "========================================")
	(print "Test Results for: " *test-suite-name*)
	(print "========================================")
	(print "Passed: " *test-passed*)
	(print "Failed: " *test-failed*)
	(when (> *test-failed* 0)
		(print "")
		(print "Failures:")
		(each! 0 -1 (lambda ((name msg))
			(print "  " name ": " msg)) *test-errors*))
	(print "========================================")
	(print "")
	(if (= *test-failed* 0)
		(print "All tests passed!")
		(print "Some tests failed!"))
	(= *test-failed* 0))

(defmacro deftest-suite (name)
	; Define a test suite
	`(setq *test-suite-name* ,name))

(defmacro deftest (name &rest body)
	; Define a test case
	`(progn
		(setq *test-name* ,name)
		(when *test-verbose*
			(print "Running test: " ,name))
		~body))

(defun test-assert-impl (condition msg)
	; Internal assertion implementation
	(if condition
		(setq *test-passed* (inc *test-passed*))
		(progn
			(setq *test-failed* (inc *test-failed*))
			(push *test-errors* (list *test-name* msg))
			(when *test-verbose*
				(print "  FAILED: " msg)))))

(defmacro assert-true (expr &optional msg)
	; Assert that expression is true
	(defq msg-str (if msg msg (str expr)))
	`(test-assert-impl ,expr (cat "Expected true: " ,msg-str)))

(defmacro assert-false (expr &optional msg)
	; Assert that expression is false
	(defq msg-str (if msg msg (str expr)))
	`(test-assert-impl (not ,expr) (cat "Expected false: " ,msg-str)))

(defmacro assert-eq (expected actual &optional msg)
	; Assert that two values are equal
	(defq msg-str (if msg msg (cat "Expected: " (str expected) " Actual: " (str actual))))
	`(test-assert-impl (= ,expected ,actual) ,msg-str))

(defmacro assert-neq (expected actual &optional msg)
	; Assert that two values are not equal
	(defq msg-str (if msg msg (cat "Expected not equal: " (str expected) " vs " (str actual))))
	`(test-assert-impl (/= ,expected ,actual) ,msg-str))

(defmacro assert-nil (expr &optional msg)
	; Assert that expression is nil
	(defq msg-str (if msg msg (str expr)))
	`(test-assert-impl (nil? ,expr) (cat "Expected nil: " ,msg-str)))

(defmacro assert-not-nil (expr &optional msg)
	; Assert that expression is not nil
	(defq msg-str (if msg msg (str expr)))
	`(test-assert-impl (not (nil? ,expr)) (cat "Expected not nil: " ,msg-str)))

(defmacro run-test-suite (&rest tests)
	; Run a series of tests
	`(progn
		(test-reset)
		~tests
		(test-report)))
