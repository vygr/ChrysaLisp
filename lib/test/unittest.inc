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

;; Numeric comparison assertions

(defmacro assert-greater (actual expected &optional msg)
	; Assert that actual > expected
	(defq msg-str (if msg msg (cat (str actual) " should be > " (str expected))))
	`(test-assert-impl (> ,actual ,expected) ,msg-str))

(defmacro assert-less (actual expected &optional msg)
	; Assert that actual < expected
	(defq msg-str (if msg msg (cat (str actual) " should be < " (str expected))))
	`(test-assert-impl (< ,actual ,expected) ,msg-str))

(defmacro assert-greater-eq (actual expected &optional msg)
	; Assert that actual >= expected
	(defq msg-str (if msg msg (cat (str actual) " should be >= " (str expected))))
	`(test-assert-impl (>= ,actual ,expected) ,msg-str))

(defmacro assert-less-eq (actual expected &optional msg)
	; Assert that actual <= expected
	(defq msg-str (if msg msg (cat (str actual) " should be <= " (str expected))))
	`(test-assert-impl (<= ,actual ,expected) ,msg-str))

(defmacro assert-close (actual expected tolerance &optional msg)
	; Assert that actual is within tolerance of expected (for floats)
	(defq msg-str (if msg msg (cat (str actual) " should be within " (str tolerance) " of " (str expected))))
	`(test-assert-impl (<= (abs (- ,actual ,expected)) ,tolerance) ,msg-str))

;; Collection assertions

(defmacro assert-contains (collection item &optional msg)
	; Assert that collection contains item
	(defq msg-str (if msg msg (cat "Collection should contain " (str item))))
	`(test-assert-impl (some! 0 -1 (lambda (x) (= x ,item)) ,collection) ,msg-str))

(defmacro assert-not-contains (collection item &optional msg)
	; Assert that collection does not contain item
	(defq msg-str (if msg msg (cat "Collection should not contain " (str item))))
	`(test-assert-impl (not (some! 0 -1 (lambda (x) (= x ,item)) ,collection)) ,msg-str))

(defmacro assert-empty (collection &optional msg)
	; Assert that collection is empty
	(defq msg-str (if msg msg "Collection should be empty"))
	`(test-assert-impl (or (nil? ,collection) (= (length ,collection) 0)) ,msg-str))

(defmacro assert-not-empty (collection &optional msg)
	; Assert that collection is not empty
	(defq msg-str (if msg msg "Collection should not be empty"))
	`(test-assert-impl (and (not (nil? ,collection)) (> (length ,collection) 0)) ,msg-str))

(defmacro assert-length (collection expected-length &optional msg)
	; Assert that collection has expected length
	(defq msg-str (if msg msg (cat "Expected length " (str expected-length))))
	`(test-assert-impl (= (length ,collection) ,expected-length) ,msg-str))

;; String assertions

(defmacro assert-starts-with (str prefix &optional msg)
	; Assert that string starts with prefix
	(defq msg-str (if msg msg (cat (str str) " should start with " (str prefix))))
	`(test-assert-impl (starts-with ,prefix ,str) ,msg-str))

(defmacro assert-ends-with (str suffix &optional msg)
	; Assert that string ends with suffix
	(defq msg-str (if msg msg (cat (str str) " should end with " (str suffix))))
	`(test-assert-impl (ends-with ,suffix ,str) ,msg-str))

(defmacro assert-contains-str (str substring &optional msg)
	; Assert that string contains substring
	(defq msg-str (if msg msg (cat (str str) " should contain " (str substring))))
	`(test-assert-impl (find ,substring ,str) ,msg-str))

;; Exception/Error assertions

(defmacro assert-throws (expr &optional msg)
	; Assert that expression throws an error/exception
	(defq msg-str (if msg msg (cat "Expected " (str expr) " to throw an error")))
	`(defq threw-error :nil)
	`(catch
		(progn ,expr (setq threw-error :nil))
		(setq threw-error :t))
	`(test-assert-impl threw-error ,msg-str))

;; Type assertions

(defmacro assert-type (expr expected-type &optional msg)
	; Assert that expression has expected type
	; expected-type can be 'str, 'num, 'list, etc.
	(defq msg-str (if msg msg (cat (str expr) " should be of type " (str expected-type))))
	`(test-assert-impl
		(cond
			((= ,expected-type 'str) (str? ,expr))
			((= ,expected-type 'num) (num? ,expr))
			((= ,expected-type 'list) (list? ,expr))
			((= ,expected-type 'env) (env? ,expr))
			(:t :nil))
		,msg-str))

(defmacro run-test-suite (&rest tests)
	; Run a series of tests
	`(progn
		(test-reset)
		~tests
		(test-report)))
