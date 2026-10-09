;the message for a square of the picture, its answer, and what the app
;and its children both need

(import "lib/math/vector.inc")
(import "lib/task/jobs.inc")

;a square for a child to work out, the size of the whole picture, the
;key of the shared pixels it is drawn on, 0 if there are none, how many
;turns a point is given to get out of the set, and where in the set the
;picture is
(structure +rect +job_size
	(int x y x1 y1)
	(long w h shared top)
	(real cx cy z))

;its answer. The square, and the part of it that is all inside the set,
;if inside is not 0. drawn says the child has drawn it on the shared
;pixels. If it has not, and not all of it is inside, the colour of every
;pixel follows, an int each
(structure +rect_reply +job_reply_size
	(int x y x1 y1)
	(int ix iy ix1 iy1)
	(int inside drawn))

;math

(defun real-offset (x w z)
	(* z (/ (* (- x (/ w +real_2)) +real_2) w)))

;the colour of each depth. They go round and round, deep blue, to white,
;to gold, to near black, and a depth is so far round them as the base and
;its root times the rate, so the bands are no thinner deep in than they
;are far out, and what is far from the set is the deep blue. The
;light on a pixel is the base, and the gain more or less as the ground
;faces the light or away, 256 is the colour as it is

(defq +mandel_pal_size 2048 +mandel_pal_rate 260.0 +mandel_pal_base 1560
	+mandel_light_base 232.0 +mandel_light_gain 96.0)

(defun generate-pal ()
	(defq out (cap +mandel_pal_size (list))
		stops '((0 0 7 100) (328 32 107 203) (860 237 255 255)
			(1316 255 170 0) (1756 26 6 44) (2048 0 7 100)))
	(each! (lambda ((p0 r0 g0 b0))
			(bind '(p1 r1 g1 b1) (elem-get stops (inc (!))))
			(each (lambda (i)
					;eased from one colour to the next
					(defq k (/ (n2f (- i p0)) (n2f (- p1 p0)))
						k (* k k (- 3.0 (* 2.0 k))))
					(push out (char (+ 0xff000000
						(<< (+ r0 (n2i (* k (n2f (- r1 r0))))) 16)
						(<< (+ g0 (n2i (* k (n2f (- g1 g0))))) 8)
						(+ b0 (n2i (* k (n2f (- b1 b0)))))) +int_size)))
				(range p0 p1)))
		(list stops) 0 (dec (length stops)))
	(apply (const cat) out))

(defq +mandel_pal (generate-pal))

(defun draw-rect (canvas pixels x y x1 y1 ix iy ix1 iy1 inside)
	;a square onto a canvas. pixels is the colour of each pixel of it, and
	;is not looked at if all of the square is inside the set
	(cond
		((and (/= inside 0) (= ix x) (= iy y) (= ix1 x1) (= iy1 y1))
			(.-> canvas (:set_color +argb_black) (:fbox x y (- x1 x) (- y1 y))))
		(:t ;the rings that were worked out, and the middle is not yet set
			(. canvas :tile pixels x y x1 y1)
			(when (/= inside 0)
				(.-> canvas (:set_color +argb_black) (:fbox ix iy (- ix1 ix) (- iy1 iy)))))))
