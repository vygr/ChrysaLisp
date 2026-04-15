;message structure

(import "lib/math/vector.inc")

(structure +job 0
	(long key)
	(int x y x1 y1)
	(netid reply)
	(long w h)
	(real cx cy z))

(structure +job_reply 0
	(long key)
	(int x y x1 y1)
	(int ix iy ix1 iy1)
	(int fill_value))

;math

(defun real-offset (x w z)
	(* z (/ (* (- x (/ w +real_2)) +real_2) w)))