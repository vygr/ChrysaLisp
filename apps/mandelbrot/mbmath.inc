(defq mbfp_shift 29)

(defmacro-bind mbfp-from-fixed (x)
	(<< x (- mbfp_shift fp_shift)))

(defmacro-bind mbfp-mul (x y &rest _)
	(if (= (length _) 0)
		`(>>> (* ,x ,y) mbfp_shift)
		`(mbfp-mul (>>> (* ,x ,y) mbfp_shift) ~_)))

(defun-bind mbfp-offset (x w z)
	(mbfp-mul z (/ (* (- x (/ w 2)) (mbfp-from-fixed 2.0)) w)))
