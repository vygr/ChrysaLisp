;message structure

(structure +job 0
	(long key)
	(netid reply)
	(long x y x1 y1 w h cx cy z))

;math

(defq +mbfp_shift 29)

(defmacro mbfp-from-fixed (x)
	(<< x (- +mbfp_shift +fp_shift)))

(defmacro mbfp-mul (x y &rest _)
	(if (= (length _) 0)
		`(>>> (* ,x ,y) +mbfp_shift)
		`(mbfp-mul (>>> (* ,x ,y) +mbfp_shift) ~_)))

(defun mbfp-offset (x w z)
	(mbfp-mul z (/ (* (- x (/ w 2)) (mbfp-from-fixed 2.0)) w)))
