;message structure

(import "lib/math/vector.inc")

(structure +job 0
	(long key)
	(netid reply)
	(long x y x1 y1 w h cx cy z))

;math

(defun real-offset (x w z)
	(* z (/ (* (- x (/ w +real_2)) +real_2) w)))