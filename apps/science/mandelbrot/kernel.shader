;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mandelbrot, a pixel of a square of the picture for each index.
; On the GPU the square is the whole picture, in floats of 32 bits,
; so no deeper than a float can tell one pixel from the next. On the
; CPU, (shader-vp-kernel), a child of the farm works out a square of
; it, in reals.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the colour of each depth, and the pixels of the square, row by row
(defbuffer pal :int :read)
(defbuffer out :int :write)

;the middle of the picture, the step from one pixel to the next, and
;the size of it
(definput cx :float -0.5)
(definput cy :float 0.0)
(definput step :float 0.0025)
(definput w :int 800)
(definput h :int 800)
;the square, where it is in the picture, and how wide
(definput ox :int 0)
(definput oy :int 0)
(definput ow :int 800)
;how many turns a point has to get out of the set, and the palette, so
;far round it as the base and the root of the depth times the rate
(definput top :int 256)
(definput base :int 1560)
(definput rate :float 260.0)
;the light, the base and the gain as a slope faces it or away, 256 is
;the colour as it is
(definput lbase :float 232.0)
(definput lgain :float 96.0)
;points across a pixel each way, the mean of them
(definput ss :int 2 1 4)

;no more turns than this, top is less
(defconst max_top 2048)
(defconst black 0xff000000)

(defun lg2 :float ((v :float))
	;log2 of v, 1 or more. The power of two under it, then log2 of 1 to 2
	;near enough m * (1.3465 - 0.3465 * m), m from 0 to 1, right at both
	;ends
	(defq e 0.0)
	(for (k 0 128)
		(when (< v 2.0) (break))
		(setq v (* v 0.5) e (+ e 1.0)))
	(defq m (- v 1.0))
	(+ e (* m (- 1.3465 (* 0.3465 m)))))

(defun shade :int ((x0 :float) (y0 :float))
	;the colour of a point, black if it does not get out of the set. A
	;point of the heart, or of the disc to its left, is in it
	(defq xq (- x0 0.25) q (+ (* xq xq) (* y0 y0)))
	(when (or (<= (* q (+ q xq)) (* 0.25 y0 y0))
			(<= (+ (* (+ x0 1.0) (+ x0 1.0)) (* y0 y0)) 0.0625))
		(return black))
	;z and its derivative, brought down with the one added to it before
	;it is too big for a float
	(defq xc 0.0 yc 0.0 x2 0.0 y2 0.0 dx 0.0 dy 0.0 one 1.0 n 0)
	(for (k 0 max_top)
		(when (or (>= k top) (>= (+ x2 y2) 65536.0)) (break))
		(defq ndx (+ (* 2.0 (- (* xc dx) (* yc dy))) one))
		(setq dy (* 2.0 (+ (* xc dy) (* yc dx)))
			dx ndx
			yc (+ (* 2.0 xc yc) y0)
			xc (+ (- x2 y2) x0)
			x2 (* xc xc)
			y2 (* yc yc)
			n (+ k 1))
		(when (> (+ (abs dx) (abs dy)) "100000000000000000000")
			(setq dx (* dx "0.00000000000000000001")
				dy (* dy "0.00000000000000000001")
				one (* one "0.00000000000000000001"))))
	(when (>= n top) (return black))
	;the depth with no steps, and its colour, the root of it as the depths
	;get closer together the deeper they are
	(defq depth (- (+ (float n) 1.0) (lg2 (/ (lg2 (+ x2 y2)) 16.0)))
		col (elem-get pal (logand (+ (int (* (sqrt depth) rate)) base) 2047))
		;the light, z over its derivative points out of the set, up and to
		;the left is bright
		re (+ (* xc dx) (* yc dy))
		im (- (* yc dx) (* xc dy))
		len (sqrt (+ (* re re) (* im im)))
		light lbase)
	(if (> len 0.0) (setq light (- lbase (/ (* lgain (+ re im)) len))))
	(defq l (int light)
		r (>> (* (logand (>> col 16) 255) l) 8)
		g (>> (* (logand (>> col 8) 255) l) 8)
		b (>> (* (logand col 255) l) 8))
	(if (> r 255) (setq r 255))
	(if (> g 255) (setq g 255))
	(if (> b 255) (setq b 255))
	(logior black (<< r 16) (<< g 8) b))

(defun main :void ((i :int))
	;a pixel, the mean of ss by ss points across it, so detail finer than a
	;pixel does not shimmer as the picture moves
	(defq row (/ i ow)
		x (+ ox (- i (* row ow)))
		y (+ oy row)
		sub (/ step (float ss))
		x0 (+ cx (* (- (float x) (* 0.5 (float w))) step) (* 0.5 sub) (* -0.5 step))
		y0 (+ cy (* (- (float y) (* 0.5 (float h))) step) (* 0.5 sub) (* -0.5 step))
		r 0 g 0 b 0)
	(for (j 0 4)
		(when (>= j ss) (break))
		(for (k 0 4)
			(when (>= k ss) (break))
			(defq c (shade (+ x0 (* (float k) sub)) (+ y0 (* (float j) sub))))
			(setq r (+ r (logand (>> c 16) 255))
				g (+ g (logand (>> c 8) 255))
				b (+ b (logand c 255)))))
	(defq n (* ss ss))
	(elem-set out i (logior black (<< (/ r n) 16) (<< (/ g n) 8) (/ b n))))
