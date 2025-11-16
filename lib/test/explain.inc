;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Testing Framework - Result Explanation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Just as important as defining and running the tests is
;; understanding the results. FiveAM provides the function EXPLAIN
;; which prints a human readable summary (number passed, number
;; failed, what failed and why, etc.) of a list of test results.
;;
;; Copyright (c) 2002-2003, Edward Marco Baringer
;; Ported to ChrysaLisp 2025
;; See test.inc for full license details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *verbose-failures* :t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result Partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun partition-results (results)
	; (partition-results results) -> (num-total passed failed skipped errors)
	; Partition results by type
	(defq num-total (length results))
	(defq passed (list) failed (list) skipped (list) errors (list))

	(each (lambda (result)
		(defq type (. result :find :type))
		(cond
			((eql type :passed) (push passed result))
			((eql type :failed) (push failed result))
			((eql type :skipped) (push skipped result))
			((eql type :error) (push errors result))))
		results)

	(list num-total passed failed skipped errors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explanation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-summary (stream num-total passed failed skipped errors)
	; (print-summary stream total passed failed skipped errors)
	; Print test results summary
	(defq num-passed (length passed))
	(defq num-failed (length failed))
	(defq num-skipped (length skipped))
	(defq num-errors (length errors))

	;; Print summary line
	(write stream (ascii-char 10))
	(write stream (ascii-char 10))

	(when (= num-total 0)
		(write stream "Didn't run anything...huh?" (ascii-char 10))
		(return :nil))

	(write stream (cat "Did " (str num-total) " check"
		(if (> num-total 1) "s" "") "." (ascii-char 10)))

	;; Calculate percentages
	(defq passed% (if (> num-total 0) (/ (* num-passed 100) num-total) 0))
	(defq failed% (if (> num-total 0) (/ (* num-failed 100) num-total) 0))
	(defq skipped% (if (> num-total 0) (/ (* num-skipped 100) num-total) 0))
	(defq errors% (if (> num-total 0) (/ (* num-errors 100) num-total) 0))

	(write stream (cat "   Pass: " (str num-passed) " (" (str passed%) "%)" (ascii-char 10)))
	(write stream (cat "   Skip: " (str num-skipped) " (" (str skipped%) "%)" (ascii-char 10)))
	(write stream (cat "   Fail: " (str num-failed) " (" (str failed%) "%)" (ascii-char 10)))
	(when (> num-errors 0)
		(write stream (cat "   Error: " (str num-errors) " (" (str errors%) "%)" (ascii-char 10))))
	(write stream (ascii-char 10)))

(defun print-failure-details (stream failures)
	; (print-failure-details stream failures)
	; Print detailed failure information
	(when (> (length failures) 0)
		(write stream "Failure Details:" (ascii-char 10))
		(each (lambda (failure)
			(write stream "--------------------------------" (ascii-char 10))
			(defq test-name (. failure :find :test-case))
			(defq reason (. failure :find :reason))
			(defq test-expr (. failure :find :test-expr))

			(write stream (cat "Test: " (str test-name) (ascii-char 10)))
			(write stream (cat "   " reason (ascii-char 10)))

			(when (and *verbose-failures* test-expr (not (eql test-expr :nil)))
				(write stream (cat "   Expression: " (str test-expr) (ascii-char 10))))

			(write stream "--------------------------------" (ascii-char 10)))
			failures)
		(write stream (ascii-char 10))))

(defun print-skip-details (stream skipped)
	; (print-skip-details stream skipped)
	; Print detailed skip information
	(when (> (length skipped) 0)
		(write stream "Skip Details:" (ascii-char 10))
		(each (lambda (skip)
			(defq test-name (. skip :find :test-case))
			(defq reason (. skip :find :reason))

			(write stream (cat "Test: " (str test-name) " - " reason (ascii-char 10))))
			skipped)
		(write stream (ascii-char 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Explain Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun explain (results &optional stream)
	; (explain results [stream]) -> :t | :nil
	; Print a detailed explanation of test results
	; Returns :t if all tests passed, :nil otherwise
	(unless stream
		(setq stream *test-dribble*))

	;; Partition results
	(defq (num-total passed failed skipped errors) (partition-results results))

	;; Combine failed and errors for reporting
	(defq all-failures (cat failed errors))

	;; Print summary
	(print-summary stream num-total passed all-failures skipped errors)

	;; Print failure details
	(print-failure-details stream all-failures)

	;; Print skip details
	(print-skip-details stream skipped)

	;; Flush output
	(stream-flush stream)

	;; Return success status
	(= 0 (length all-failures)))

(defun explain-simple (results &optional stream)
	; (explain-simple results [stream]) -> :t | :nil
	; Print a simple one-line summary of test results
	(unless stream
		(setq stream *test-dribble*))

	(defq (num-total passed failed skipped errors) (partition-results results))
	(defq num-passed (length passed))
	(defq num-failed (+ (length failed) (length errors)))
	(defq num-skipped (length skipped))

	(write stream (ascii-char 10))
	(write stream (cat "Ran " (str num-total) " checks, "
		(str num-passed) " passed"))

	(when (> num-skipped 0)
		(write stream (cat ", " (str num-skipped) " skipped")))

	(write stream (cat " and " (str num-failed) " failed." (ascii-char 10)))
	(stream-flush stream)

	(= 0 num-failed))
