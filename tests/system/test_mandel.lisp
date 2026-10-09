(report-header "Mandelbrot: the colour of a point, worked out by the app's native function")

(import "gui/lisp.inc")
(import "apps/science/mandelbrot/app.inc")
(jit "apps/science/mandelbrot/" "lisp.vp" '("shade"))
(ffi "apps/science/mandelbrot/shade" mb_shade)

(defun mb-at (x y &optional top)
	(mb_shade (n2r x) (n2r y) (ifn top 256) +mandel_pal))

(defun mb-far (a b)
	;how far apart two colours are, the most of red, green and blue
	(reduce (lambda (m sh) (max m (abs (- (logand (>> a sh) 0xff) (logand (>> b sh) 0xff)))))
		'(0 8 16) 0))

;the palette
(assert-eq "the palette is an int for each colour" (* +mandel_pal_size +int_size) (length +mandel_pal))
(assert-true "and every one of them is solid"
	(every (# (= (logand (get-int +mandel_pal (* %0 +int_size)) 0xff000000) 0xff000000))
		(range 0 +mandel_pal_size)))

;inside the set
(assert-eq "the middle of the heart is inside" -1 (mb-at 0.0 0.0))
(assert-eq "so is the disc to its left" -1 (mb-at -1.0 0.0))
(assert-eq "and a bud that only the loop finds" -1 (mb-at -0.125 0.75))

;outside it
(defq mb_out (mb-at 1.0 1.0))
(assert-true "a point far off is a colour" (/= mb_out -1))
(assert-eq "that is solid" 0xff000000 (logand mb_out 0xff000000))
(assert-true "a point by the edge is too" (/= (mb-at -0.75 0.1) -1))

;the depth has no steps in it, two points close by are close in colour,
;from near the edge to far off
(assert-true "the colour has no steps in it"
	(every (lambda (i)
		(defq x (+ 0.4 (* (n2f i) 0.002)))
		(< (mb-far (mb-at x 0.5) (mb-at (+ x 0.002) 0.5)) 8)) (range 0 400)))

;more turns find more of the outside
(defq mb_deep '(-0.74543 0.11301))
(assert-eq "a point that takes a while is inside if not given the turns" -1
	(mb-at (first mb_deep) (second mb_deep) 16))
(assert-true "and outside if it is" (/= (mb-at (first mb_deep) (second mb_deep) 2048) -1))
