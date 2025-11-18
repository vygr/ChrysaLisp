;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Plot Library Test Suite
; Tests data processing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/plot/data.inc")

(defq *test_count* 0)
(defq *pass_count* 0)
(defq *fail_count* 0)

(defun assert-equal (name actual expected &optional tolerance)
	; Test assertion with optional tolerance for floating point
	(setq *test_count* (inc *test_count*))
	(defq tolerance (opt tolerance 0.0001)
		  passed (cond
		  	((and (num? actual) (num? expected))
		  		(< (abs (- actual expected)) tolerance))
		  	(:t (= actual expected))))
	(cond
		(passed
			(setq *pass_count* (inc *pass_count*))
			(print "✓ " name))
		(:t
			(setq *fail_count* (inc *fail_count*))
			(print "✗ " name " - Expected: " expected " Got: " actual))))

(defun assert-list-equal (name actual expected &optional tolerance)
	; Test list equality with optional tolerance
	(setq *test_count* (inc *test_count*))
	(defq tolerance (opt tolerance 0.0001)
		  passed (and (= (length actual) (length expected))
		  	(every (lambda (i)
		  		(< (abs (- (elem i actual) (elem i expected))) tolerance))
		  		(range 0 (length actual)))))
	(cond
		(passed
			(setq *pass_count* (inc *pass_count*))
			(print "✓ " name))
		(:t
			(setq *fail_count* (inc *fail_count*))
			(print "✗ " name " - Expected: " expected " Got: " actual))))

(defun print-summary ()
	(print)
	(print "========================================")
	(print "Test Summary:")
	(print "  Total:  " *test_count*)
	(print "  Passed: " *pass_count*)
	(print "  Failed: " *fail_count*)
	(print "========================================")
	(cond
		((= *fail_count* 0)
			(print "✓ All tests passed!")
			:t)
		(:t
			(print "✗ Some tests failed")
			:nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Statistical Function Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Testing Statistical Functions...")
(print)

; Test plot-mean
(assert-equal "mean of 1-5" (plot-mean '(1.0 2.0 3.0 4.0 5.0)) 3.0)
(assert-equal "mean of empty list" (plot-mean '()) 0.0)
(assert-equal "mean of single value" (plot-mean '(7.0)) 7.0)
(assert-equal "mean of negative values" (plot-mean '(-2.0 -4.0 -6.0)) -4.0)

; Test plot-median
(assert-equal "median of odd length" (plot-median '(1.0 2.0 3.0 4.0 5.0)) 3.0)
(assert-equal "median of even length" (plot-median '(1.0 2.0 3.0 4.0)) 2.5)
(assert-equal "median of single value" (plot-median '(5.0)) 5.0)
(assert-equal "median of unsorted" (plot-median '(5.0 1.0 3.0 2.0 4.0)) 3.0)
(assert-equal "median of empty list" (plot-median '()) 0.0)

; Test plot-std-dev
(assert-equal "std dev of uniform data" (plot-std-dev '(5.0 5.0 5.0 5.0)) 0.0)
(assert-equal "std dev of 2,4,4,4,5,5,7,9" (plot-std-dev '(2.0 4.0 4.0 4.0 5.0 5.0 7.0 9.0)) 2.0)
(assert-equal "std dev of empty list" (plot-std-dev '()) 0.0)
(assert-equal "std dev of single value" (plot-std-dev '(5.0)) 0.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Transformation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Data Transformation...")
(print)

; Test plot-normalize
(assert-list-equal "normalize 0,5,10 to [0,1]"
	(plot-normalize '(0.0 5.0 10.0))
	'(0.0 0.5 1.0))

(assert-list-equal "normalize to custom range"
	(plot-normalize '(0.0 5.0 10.0) 0.0 100.0)
	'(0.0 50.0 100.0))

(assert-list-equal "normalize uniform data"
	(plot-normalize '(5.0 5.0 5.0))
	'(0.0 0.0 0.0))

; Test plot-linspace
(assert-list-equal "linspace 0-1 with 5 points"
	(plot-linspace 0.0 1.0 5)
	'(0.0 0.25 0.5 0.75 1.0))

(assert-list-equal "linspace 0-10 with 3 points"
	(plot-linspace 0.0 10.0 3)
	'(0.0 5.0 10.0))

(assert-equal "linspace with 1 point"
	(length (plot-linspace 0.0 10.0 1))
	1)

; Test plot-resample
(defq test_data '((0.0 0.0) (1.0 1.0) (2.0 2.0) (3.0 3.0) (4.0 4.0)))
(assert-equal "resample to fewer points"
	(length (plot-resample test_data 3))
	3)

(assert-equal "resample preserves first point"
	(first (first (plot-resample test_data 3)))
	0.0)

(assert-equal "resample preserves last point"
	(first (first (last (plot-resample test_data 3))))
	4.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Combination Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Data Combination...")
(print)

; Test plot-combine-xy
(defq xs '(1.0 2.0 3.0))
(defq ys '(4.0 5.0 6.0))
(defq combined (plot-combine-xy xs ys))

(assert-equal "combine-xy length" (length combined) 3)
(assert-equal "combine-xy first x" (first (first combined)) 1.0)
(assert-equal "combine-xy first y" (second (first combined)) 4.0)
(assert-equal "combine-xy last x" (first (last combined)) 3.0)
(assert-equal "combine-xy last y" (second (last combined)) 6.0)

; Test plot-add-noise
(defq clean_data '((1.0 5.0) (2.0 5.0) (3.0 5.0)))
(defq noisy_data (plot-add-noise clean_data 0.1))

(assert-equal "add-noise preserves length" (length noisy_data) 3)
(assert-equal "add-noise preserves x values" (first (first noisy_data)) 1.0)
; Y values should be different but we can't test exact values due to randomness

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Analysis Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Data Analysis...")
(print)

; Test plot-fit-linear with perfect line
(defq line_data '((0.0 0.0) (1.0 2.0) (2.0 4.0) (3.0 6.0)))
(defq fit (plot-fit-linear line_data))

(assert-equal "linear fit slope" (first fit) 2.0)
(assert-equal "linear fit intercept" (second fit) 0.0 0.01)

; Test with horizontal line
(defq flat_data '((0.0 5.0) (1.0 5.0) (2.0 5.0)))
(defq flat_fit (plot-fit-linear flat_data))

(assert-equal "flat line slope" (first flat_fit) 0.0 0.01)
(assert-equal "flat line intercept" (second flat_fit) 5.0 0.01)

; Test plot-apply-linear-fit
(defq fit_line (plot-apply-linear-fit line_data fit))

(assert-equal "apply fit length" (length fit_line) (length line_data))
(assert-equal "apply fit first y" (second (first fit_line)) 0.0)
(assert-equal "apply fit last y" (second (last fit_line)) 6.0)

; Test plot-find-peaks
(defq peak_data '((0.0 1.0) (1.0 3.0) (2.0 2.0) (3.0 5.0) (4.0 1.0)))
(defq peaks (plot-find-peaks peak_data 2.5))

(assert-equal "find peaks count" (length peaks) 2)
(assert-equal "first peak index" (first peaks) 1)
(assert-equal "second peak index" (second peaks) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Box Plot Statistics Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Box Plot Statistics...")
(print)

; Test plot-quartiles
(defq values '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
(defq quartiles (plot-quartiles values))

(assert-equal "quartiles length" (length quartiles) 3)
(assert-equal "quartiles median" (second quartiles) 5.0)

; Test plot-box-stats
(defq box_stats (plot-box-stats values))

(assert-equal "box stats length" (length box_stats) 6)
(assert-equal "box stats min" (first box_stats) 1.0)
(assert-equal "box stats max" (elem 4 box_stats) 9.0)
(assert-equal "box stats median" (elem 2 box_stats) 5.0)

; Test with outliers
(defq outlier_values '(1.0 2.0 3.0 4.0 5.0 6.0 100.0))  ; 100.0 is outlier
(defq outlier_stats (plot-box-stats outlier_values))
(defq outliers (last outlier_stats))

(assert-equal "outlier detection" (> (length outliers) 0) :t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Edge Cases and Error Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Edge Cases...")
(print)

; Empty lists
(assert-equal "mean empty" (plot-mean '()) 0.0)
(assert-equal "median empty" (plot-median '()) 0.0)
(assert-equal "std-dev empty" (plot-std-dev '()) 0.0)
(assert-equal "normalize empty length" (length (plot-normalize '())) 0)

; Single element
(assert-equal "mean single" (plot-mean '(5.0)) 5.0)
(assert-equal "median single" (plot-median '(5.0)) 5.0)
(assert-equal "std-dev single" (plot-std-dev '(5.0)) 0.0)

; Negative values
(assert-equal "mean negative" (plot-mean '(-5.0 -10.0 -15.0)) -10.0)
(assert-equal "median negative" (plot-median '(-5.0 -10.0 -15.0)) -10.0)

; Zero values
(assert-equal "mean zeros" (plot-mean '(0.0 0.0 0.0)) 0.0)
(assert-equal "median zeros" (plot-median '(0.0 0.0 0.0)) 0.0)
(assert-equal "std-dev zeros" (plot-std-dev '(0.0 0.0 0.0)) 0.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate Data Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "Testing Data Generation...")
(print)

; Test plot-function
(defun square (x) (* x x))
(defq func_data (plot-function square 0.0 3.0 4))

(assert-equal "function data length" (length func_data) 4)
(assert-equal "function data first x" (first (first func_data)) 0.0)
(assert-equal "function data first y" (second (first func_data)) 0.0)
(assert-equal "function data last x" (first (last func_data)) 3.0)
(assert-equal "function data last y" (second (last func_data)) 9.0)

; Test plot-smooth-data
(defq noisy '((0.0 1.0) (1.0 5.0) (2.0 2.0) (3.0 4.0) (4.0 3.0)))
(defq smoothed (plot-smooth-data noisy 3))

(assert-equal "smooth preserves length" (length smoothed) (length noisy))
(assert-equal "smooth preserves x values" (first (first smoothed)) 0.0)

; Smoothed y value at index 2 should be average of window
(defq mid_smooth_y (second (elem 2 smoothed)))
(defq expected_mid (/ (+ 5.0 2.0 4.0) 3.0))
(assert-equal "smooth middle value" mid_smooth_y expected_mid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print-summary)
