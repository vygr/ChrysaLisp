(report-header "Mandelbrot: the colour of a point, worked out by the app's kernel as native code")

(import "gui/lisp.inc")
(import "lib/gpu/vp.inc")
(import "apps/science/mandelbrot/app.inc")

(defq mb_pal (mandel-pal 0) mb_program (shader-load "apps/science/mandelbrot/kernel.shader")
	mb_native (shader-vp-kernel mb_program) mb_inside 0xff000000)

(defun mb-at (x y &optional top)
	;a picture of one pixel, with no width and one point, is the point
	(defq out (str-alloc 4))
	(shader-vp-run mb_program mb_native
		(shader-vp-frame mb_program mb_native (list (list 'cx (n2r x)) (list 'cy (n2r y))
			'(step 0.0) '(w 1) '(h 1) '(ow 1) (list 'top (ifn top 256)) '(ss 1)))
		0 1 (list mb_pal out))
	(get-uint out 0))

(defun mb-far (a b)
	;how far apart two colours are, the most of red, green and blue
	(reduce (lambda (m sh) (max m (abs (- (logand (>> a sh) 0xff) (logand (>> b sh) 0xff)))))
		'(0 8 16) 0))

;the palettes, each made from its stops
(each (lambda (p)
		(defq name (first p) pal (mandel-pal (!)))
		(assert-eq (cat "the palette " name " is an int for each colour") (* +mandel_pal_size +int_size) (length pal))
		(assert-true (cat "and every one of them is solid")
			(every (# (= (logand (get-int pal (* %0 +int_size)) 0xff000000) 0xff000000))
				(range 0 +mandel_pal_size))))
	+mandel_palettes)
(assert-eq "the first is made once" (mandel-pal 0) mb_pal)

;inside the set
(assert-eq "the middle of the heart is inside" mb_inside (mb-at 0.0 0.0))
(assert-eq "so is the disc to its left" mb_inside (mb-at -1.0 0.0))
(assert-eq "and a bud that only the loop finds" mb_inside (mb-at -0.125 0.75))

;outside it
(defq mb_out (mb-at 1.0 1.0))
(assert-true "a point far off is a colour" (/= mb_out mb_inside))
(assert-eq "that is solid" 0xff000000 (logand mb_out 0xff000000))
(assert-true "a point by the edge is too" (/= (mb-at -0.75 0.1) mb_inside))

;the depth has no steps in it, two points close by are close in colour,
;from near the edge to far off
(assert-true "the colour has no steps in it"
	(every (lambda (i)
		(defq x (+ 0.4 (* (n2f i) 0.002)))
		(< (mb-far (mb-at x 0.5) (mb-at (+ x 0.002) 0.5)) 8)) (range 0 400)))

;more turns find more of the outside
(defq mb_deep '(-0.74543 0.11301))
(assert-eq "a point that takes a while is inside if not given the turns" mb_inside
	(mb-at (first mb_deep) (second mb_deep) 16))
(assert-true "and outside if it is" (/= (mb-at (first mb_deep) (second mb_deep) 2048) mb_inside))
