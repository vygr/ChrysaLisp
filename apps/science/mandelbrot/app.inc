;the message for a square of the picture, its answer, and what the app
;and its children both need

(import "lib/math/vector.inc")
(import "lib/task/jobs.inc")

;a square for a child to work out, the size of the whole picture, the
;key of the shared pixels it is drawn on, 0 if there are none, and
;where in the set the picture is
(structure +rect +job_size
	(int x y x1 y1)
	(long w h shared)
	(real cx cy z))

;its answer. The square, the part of it that is all one depth, and that
;depth, -1 if no part is. drawn says the child has drawn it on the
;shared pixels. If it has not, and the fill is not the whole of it, the
;depth of every pixel follows, a byte each
(structure +rect_reply +job_reply_size
	(int x y x1 y1)
	(int ix iy ix1 iy1)
	(int fill_value drawn))

;math

(defun real-offset (x w z)
	(* z (/ (* (- x (/ w +real_2)) +real_2) w)))

;the colour of each depth

(defun generate-lut ()
	(defq lut (list) i -1)
	(while (< (++ i) 255)
		(defq
			; Frequency
			f 0.1 fi (n2f i)
			; Phase shifts for R, G, B
			r (n2i (+ (* (sin (* f fi)) 127.0) 128.0))
			g (n2i (+ (* (sin (+ (* f fi) 2.0)) 127.0) 128.0))
			b (n2i (+ (* (sin (+ (* f fi) 4.0)) 127.0) 128.0)))
		; Combine into ARGB, force Alpha 0xFF
		(push lut (char (+ 0xFF000000 (<< r 16) (<< g 8) b) +int_size)))
	; Set index 255 to Black
	(push lut (char +argb_black +int_size)))

(defq +mandel_lut `',(generate-lut))

(defun draw-rect (canvas depths x y x1 y1 ix iy ix1 iy1 fill_value)
	;a square onto a canvas. depths is a byte for each pixel of it, and is
	;not looked at if the fill is the whole of the square
	(cond
		((and (/= fill_value -1) (= ix x) (= iy y) (= ix1 x1) (= iy1 y1))
			;fill covers the WHOLE area, skip :tile completely!
			(.-> canvas
				(:set_color (get-int (elem-get +mandel_lut fill_value) 0))
				(:fbox x y (- x1 x) (- y1 y))))
		(:t ;tile the full buffer (draws computed perimeter rings + uninitialized interior garbage)
			(. canvas :tile (apply (const cat) (map (# (elem-get +mandel_lut (code %0))) depths)) x y x1 y1)
			;overwrite the uninitialized interior garbage with any solid fill!
			(when (/= fill_value -1)
				(.-> canvas
					(:set_color (get-int (elem-get +mandel_lut fill_value) 0))
					(:fbox ix iy (- ix1 ix) (- iy1 iy)))))))
