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
	(long w h shared top pal)
	(real cx cy z))

;its answer. The square, and the part of it that is all inside the set,
;if inside is not 0. drawn says the child has drawn it on the shared
;pixels. If it has not, and not all of it is inside, the colour of every
;pixel follows, an int each
(structure +rect_reply +job_reply_size
	(int x y x1 y1)
	(int ix iy ix1 iy1)
	(int inside drawn))

;the colour of each depth. They go round and round, in the first of the
;palettes deep blue, to white, to gold, to near black, and a depth is so
;far round them as the base and its root times the rate, so the bands are
;no thinner deep in than they are far out, and what is far from the set is
;the colour at the base. The light on a pixel is the base, and the gain
;more or less as the ground faces the light or away, 256 is the colour as
;it is

(defq +mandel_pal_size 2048 +mandel_pal_rate 260.0 +mandel_pal_base 1560
	+mandel_light_base 232.0 +mandel_light_gain 96.0
	;points a pixel each way, the mean of them, so detail finer than a pixel
	;does not shimmer as the picture moves
	+mandel_samples 2)

;each palette, its name, the colour of its chip on the toolbar, and its
;stops, where round it each colour is and its red green and blue. The
;first is the one there has always been
(defq +mandel_palettes ''(
	("classic" 0xffeda700 ((0 0 7 100) (328 32 107 203) (860 237 255 255)
		(1316 255 170 0) (1756 26 6 44) (2048 0 7 100)))
	("fire" 0xffff6a00 ((0 0 0 0) (400 120 0 0) (900 230 60 0)
		(1350 255 190 30) (1700 255 255 220) (2048 0 0 0)))
	("ice" 0xff30c8e8 ((0 0 0 30) (500 0 60 120) (1000 0 170 200)
		(1450 200 250 255) (1800 40 90 160) (2048 0 0 30)))
	("ink" 0xffb0b0b0 ((0 10 10 10) (800 110 110 110) (1300 245 245 245)
		(1700 70 70 70) (2048 10 10 10)))
	("sunset" 0xffe0408a ((0 25 0 50) (500 120 20 110) (1000 230 60 90)
		(1450 255 160 60) (1800 255 235 170) (2048 25 0 50)))
	("forest" 0xff5aa028 ((0 0 20 5) (500 20 80 30) (1000 90 160 40)
		(1450 200 230 120) (1800 250 245 210) (2048 0 20 5)))
	("spectrum" 0xffa040ff ((0 255 0 0) (341 255 255 0) (683 0 255 0)
		(1024 0 255 255) (1365 0 0 255) (1707 255 0 255) (2048 255 0 0)))))

(defun generate-pal (stops)
	; (generate-pal stops) -> str, +mandel_pal_size argb ints
	(defq out (cap +mandel_pal_size (list)))
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

;each palette as it is wanted, made once. Nothing is made as this file is
;read, the children read it too
(defq mandel_pals :nil)

(defun mandel-pal (i)
	; (mandel-pal i) -> str, the palette i, of +mandel_pal_size argb ints
	(unless mandel_pals (setq mandel_pals (Fmap 11)))
	(unless (defq p (. mandel_pals :find i))
		(. mandel_pals :insert i (setq p (generate-pal (third (elem-get +mandel_palettes i))))))
	p)

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
