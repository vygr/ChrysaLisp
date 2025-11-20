;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Testing Framework - Check Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; At the lowest level testing the system requires that certain
;; forms be evaluated and that certain post conditions are met: the
;; value returned must satisfy a certain predicate, the form must
;; (or must not) signal a certain condition, etc. In FiveAM these
;; low level operations are called 'checks' and are defined using
;; the various checking macros.
;;
;; Copyright (c) 2002-2003, Edward Marco Baringer
;; Ported to ChrysaLisp 2025
;; See test.inc for full license details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global test state
(defq *test-dribble* (io-stream 'stdout))
(defq *test-results* (list))
(defq *current-test* :nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Result Creators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-result-create (type test-expr reason &optional condition)
	; (test-result-create type expr reason [condition]) -> result
	; Create a test result hmap
	(defq result (Emap))
	(. result :insert :type type)
	(. result :insert :test-expr test-expr)
	(. result :insert :reason reason)
	(. result :insert :test-case *current-test*)
	(when condition
		(. result :insert :condition condition))
	result)

(defun test-passed? (result)
	; (test-passed? result) -> :t | :nil
	(eql :passed (. result :find :type)))

(defun test-failed? (result)
	; (test-failed? result) -> :t | :nil
	(eql :failed (. result :find :type)))

(defun test-skipped? (result)
	; (test-skipped? result) -> :t | :nil
	(eql :skipped (. result :find :type)))

(defun add-result (type test-expr reason &optional condition)
	; (add-result type expr reason [condition])
	; Add a test result to the global results list
	(defq result (test-result-create type test-expr reason condition))
	(push *test-results* result)

	;; Print progress indicator
	(cond
		((eql type :passed) (write *test-dribble* "."))
		((eql type :failed) (write *test-dribble* "f"))
		((eql type :skipped) (write *test-dribble* "s"))
		((eql type :error) (write *test-dribble* "X")))
	(stream-flush *test-dribble*)
	result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is (test &optional reason)
	; (is test [reason])
	; The DWIM checking operator.
	; If TEST returns a true value a test-passed result is generated,
	; otherwise a test-failure result is generated.
	(defq test-sym (gensym))
	(if reason
		`(let ((,test-sym ,test))
			(if ,test-sym
				(add-result :passed ',test ,reason)
				(add-result :failed ',test ,reason)))
		`(let ((,test-sym ,test))
			(if ,test-sym
				(add-result :passed ',test "Test passed")
				(add-result :failed ',test
					(cat "Test failed: " (str ',test)))))))

(defmacro is-true (condition &optional reason)
	; (is-true condition [reason])
	; Like IS this check generates a pass if CONDITION returns true
	; and a failure if CONDITION returns false.
	(defq test-sym (gensym))
	(if reason
		`(let ((,test-sym ,condition))
			(if ,test-sym
				(add-result :passed ',condition ,reason)
				(add-result :failed ',condition ,reason)))
		`(let ((,test-sym ,condition))
			(if ,test-sym
				(add-result :passed ',condition "Condition is true")
				(add-result :failed ',condition
					(cat "Condition is not true: " (str ',condition)))))))

(defmacro is-false (condition &optional reason)
	; (is-false condition [reason])
	; Generates a pass if CONDITION returns false, generates a
	; failure otherwise.
	(defq test-sym (gensym))
	(if reason
		`(let ((,test-sym ,condition))
			(if ,test-sym
				(add-result :failed ',condition ,reason)
				(add-result :passed ',condition ,reason)))
		`(let ((,test-sym ,condition))
			(if ,test-sym
				(add-result :failed ',condition
					(cat "Condition is true (should be false): " (str ',condition)))
				(add-result :passed ',condition "Condition is false")))))

(defmacro is-eq (expected actual &optional reason)
	; (is-eq expected actual [reason])
	; Test that expected equals actual using eql
	(defq exp-sym (gensym) act-sym (gensym))
	(if reason
		`(let ((,exp-sym ,expected) (,act-sym ,actual))
			(if (eql ,exp-sym ,act-sym)
				(add-result :passed '(eql ,expected ,actual) ,reason)
				(add-result :failed '(eql ,expected ,actual)
					(cat ,reason " - Expected: " (str ,exp-sym) ", Got: " (str ,act-sym)))))
		`(let ((,exp-sym ,expected) (,act-sym ,actual))
			(if (eql ,exp-sym ,act-sym)
				(add-result :passed '(eql ,expected ,actual)
					(cat "Values are equal: " (str ,exp-sym)))
				(add-result :failed '(eql ,expected ,actual)
					(cat "Values not equal - Expected: " (str ,exp-sym) ", Got: " (str ,act-sym)))))))

(defmacro is-equal (expected actual &optional reason)
	; (is-equal expected actual [reason])
	; Test that expected equals actual using equal
	(defq exp-sym (gensym) act-sym (gensym))
	(if reason
		`(let ((,exp-sym ,expected) (,act-sym ,actual))
			(if (equal ,exp-sym ,act-sym)
				(add-result :passed '(equal ,expected ,actual) ,reason)
				(add-result :failed '(equal ,expected ,actual)
					(cat ,reason " - Expected: " (str ,exp-sym) ", Got: " (str ,act-sym)))))
		`(let ((,exp-sym ,expected) (,act-sym ,actual))
			(if (equal ,exp-sym ,act-sym)
				(add-result :passed '(equal ,expected ,actual)
					(cat "Values are equal: " (str ,exp-sym)))
				(add-result :failed '(equal ,expected ,actual)
					(cat "Values not equal - Expected: " (str ,exp-sym) ", Got: " (str ,act-sym)))))))

(defmacro skip (&optional reason)
	; (skip [reason])
	; Generates a TEST-SKIPPED result.
	`(add-result :skipped :nil ,(if reason reason "Test skipped")))

(defmacro pass (&optional reason)
	; (pass [reason])
	; Simply generate a PASS.
	`(add-result :passed :nil ,(if reason reason "Explicit pass")))

(defmacro fail (&optional reason)
	; (fail [reason])
	; Simply generate a FAIL.
	`(add-result :failed :nil ,(if reason reason "Explicit failure")))

(defmacro finishes (&rest body)
	; (finishes body ...)
	; Generates a pass if BODY executes to normal completion.
	; In other words if body signals an error this test fails.
	(defq result-sym (gensym))
	`(catch (progn ~body)
		(defq ,result-sym (add-result :passed ',body "Code finished normally"))
		(add-result :failed ',body (cat "Code failed with error: " (str _)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-results ()
	; (clear-results)
	; Clear all test results
	(setq *test-results* (list)))

(defun get-results ()
	; (get-results) -> results-list
	; Get current test results
	*test-results*)
