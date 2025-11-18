;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Plot Data Processing Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
; Statistical Functions
;;;;;;;;;;;;;;;;

(defun plot-mean (values)
	; (plot-mean values) -> mean
	; Calculate mean of values
	(cond
		((empty? values) 0.0)
		(:t (/ (reduce + values 0) (length values)))))

(defun plot-median (values)
	; (plot-median values) -> median
	; Calculate median of values
	(cond
		((empty? values) 0.0)
		(:t
			(defq sorted (sort (map (lambda (x) x) values))
				  n (length sorted)
				  mid (>>> n 1))
			(if (even? n)
				(/ (+ (elem mid sorted) (elem (dec mid) sorted)) 2)
				(elem mid sorted)))))

(defun plot-std-dev (values)
	; (plot-std-dev values) -> std_dev
	; Calculate standard deviation
	(cond
		((< (length values) 2) 0.0)
		(:t
			(defq mean (plot-mean values)
				  variance (/ (reduce +
					(map (lambda (x) (* (- x mean) (- x mean))) values) 0)
					(dec (length values))))
			(sqrt variance))))

;;;;;;;;;;;;;;;;
; Data Transformation
;;;;;;;;;;;;;;;;

(defun plot-smooth-data (data window_size)
	; (plot-smooth-data data window_size) -> smoothed_data
	; Apply moving average smoothing
	(cond
		((or (empty? data) (<= window_size 1)) data)
		(:t
			(defq result (list)
				  half (>>> window_size 1))
			(each (lambda (i)
				(defq start (max 0 (- i half))
					  end (min (length data) (+ i half 1))
					  window (slice data start end)
					  avg_y (/ (reduce + (map second window) 0) (length window))
					  x (first (elem i data)))
				(push result (list x avg_y)))
				(range 0 (length data)))
			result)))

(defun plot-normalize (values &optional min_val max_val)
	; (plot-normalize values [min_val max_val]) -> normalized_values
	; Normalize values to [min_val, max_val] range (default [0, 1])
	(defq min_val (opt min_val 0.0)
		  max_val (opt max_val 1.0))
	(cond
		((empty? values) (list))
		(:t
			(defq data_min (apply min values)
				  data_max (apply max values)
				  data_range (- data_max data_min))
			(cond
				((= data_range 0) (map (lambda (_) min_val) values))
				(:t
					(map (lambda (v)
						(+ min_val (* (- v data_min) (/ (- max_val min_val) data_range))))
						values))))))

(defun plot-resample (data num_points)
	; (plot-resample data num_points) -> resampled_data
	; Resample data to specified number of points
	(cond
		((or (empty? data) (<= num_points 0)) (list))
		((<= (length data) num_points) data)
		(:t
			(defq result (list)
				  step (/ (dec (length data)) (dec num_points)))
			(each (lambda (i)
				(defq idx (floor (* i step)))
				(push result (elem idx data)))
				(range 0 num_points))
			result)))

;;;;;;;;;;;;;;;;
; Data Generation
;;;;;;;;;;;;;;;;

(defun plot-linspace (start end num_points)
	; (plot-linspace start end num_points) -> values
	; Generate linearly spaced values
	(cond
		((<= num_points 1) (list start))
		(:t
			(defq step (/ (- end start) (dec num_points)))
			(map (lambda (i) (+ start (* i step))) (range 0 num_points)))))

(defun plot-function (func x_min x_max num_points)
	; (plot-function func x_min x_max num_points) -> data
	; Generate (x, func(x)) data points
	(defq xs (plot-linspace x_min x_max num_points))
	(map (lambda (x) (list x (func x))) xs))

;;;;;;;;;;;;;;;;
; Data Combination
;;;;;;;;;;;;;;;;

(defun plot-combine-xy (xs ys)
	; (plot-combine-xy xs ys) -> data
	; Combine x and y lists into (x y) pairs
	(map (lambda (i) (list (elem i xs) (elem i ys)))
		(range 0 (min (length xs) (length ys)))))

(defun plot-add-noise (data amplitude)
	; (plot-add-noise data amplitude) -> noisy_data
	; Add random noise to y-values
	(map (lambda (pt)
		(bind '(x y) pt)
		(list x (+ y (* amplitude (- (* 2.0 (random)) 1.0)))))
		data))

;;;;;;;;;;;;;;;;
; Data Analysis
;;;;;;;;;;;;;;;;

(defun plot-find-peaks (data threshold)
	; (plot-find-peaks data threshold) -> peak_indices
	; Find local peaks above threshold
	(cond
		((< (length data) 3) (list))
		(:t
			(defq peaks (list))
			(each (lambda (i)
				(when (and (> i 0) (< i (dec (length data))))
					(defq prev_y (second (elem (dec i) data))
						  curr_y (second (elem i data))
						  next_y (second (elem (inc i) data)))
					(when (and (> curr_y prev_y) (> curr_y next_y) (> curr_y threshold))
						(push peaks i))))
				(range 0 (length data)))
			peaks)))

(defun plot-fit-linear (data)
	; (plot-fit-linear data) -> (slope intercept)
	; Simple linear regression
	(cond
		((< (length data) 2) '(0.0 0.0))
		(:t
			(defq n (length data)
				  sum_x 0.0 sum_y 0.0 sum_xy 0.0 sum_x2 0.0)
			(each (lambda (pt)
				(bind '(x y) pt)
				(setq sum_x (+ sum_x x)
					  sum_y (+ sum_y y)
					  sum_xy (+ sum_xy (* x y))
					  sum_x2 (+ sum_x2 (* x x))))
				data)
			(defq slope (/ (- (* n sum_xy) (* sum_x sum_y))
						   (- (* n sum_x2) (* sum_x sum_x)))
				  intercept (/ (- sum_y (* slope sum_x)) n))
			(list slope intercept))))

(defun plot-apply-linear-fit (data fit)
	; (plot-apply-linear-fit data fit) -> fit_line_data
	; Generate line from linear fit
	(bind '(slope intercept) fit)
	(map (lambda (pt)
		(defq x (first pt))
		(list x (+ (* slope x) intercept)))
		data))
