;message structure for distributed rendering

(structure +job 0
	(long key)
	(netid reply)
	(long x y x1 y1 w h cx cy z)
	(long fractal_type param1 param2 param3 color_scheme))

;math - using 29-bit fixed point for precision

(defq +fp_shift 29)

(defmacro fp-from-fixed (x)
	(<< x (- +fp_shift +fp_shift)))

(defmacro fp-from-float (x)
	(<< x +fp_shift))

(defmacro fp-mul (x y &rest _)
	(if (= (length _) 0)
		`(>>> (* ,x ,y) +fp_shift)
		`(fp-mul (>>> (* ,x ,y) +fp_shift) ~_)))

(defun fp-offset (x w z)
	(fp-mul z (/ (* (- x (/ w 2)) (<< (mbfp-from-fixed 4.0) +fp_shift)) w)))

;fractal type enumeration

(enums +fractal 0
	(enum julia burning_ship newton tricorn phoenix
		lyapunov mandel3 mandel4 mandelbox))

;color scheme enumeration

(enums +color_scheme 0
	(enum classic rainbow fire ice psychedelic ocean sunset electric forest copper))

;fixed point helpers for Mandelbrot-style math

(defq +mbfp_shift 29)

(defmacro mbfp-from-fixed (x)
	(<< x (- +mbfp_shift +fp_shift)))

(defmacro mbfp-mul (x y &rest _)
	(if (= (length _) 0)
		`(>>> (* ,x ,y) +mbfp_shift)
		`(mbfp-mul (>>> (* ,x ,y) +mbfp_shift) ~_)))

(defun mbfp-offset (x w z)
	(mbfp-mul z (/ (* (- x (/ w 2)) (mbfp-from-fixed 2.0)) w)))

;color generation helpers

(defun get-color-classic (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(+ +argb_black (<< (logand iter 0xff) 16) (<< (logand (<< iter 1) 0xff) 8) (logand (<< iter 2) 0xff))))

(defun get-color-rainbow (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (+ 128 (* 127 (abs (- (/ (* t 6) 255) 3)))) 0xff)
			g (logand (+ 128 (* 127 (abs (- (/ (* (+ t 128) 6) 255) 3)))) 0xff)
			b (logand (+ 128 (* 127 (abs (- (/ (* (+ t 192) 6) 255) 3)))) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-fire (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (min 255 (* t 2))
			g (logand (/ (* t 3) 2) 0xff)
			b (logand (/ t 4) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-ice (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (/ t 4) 0xff)
			g (logand (/ (* t 3) 2) 0xff)
			b (min 255 (* t 2)))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-psychedelic (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			phase (/ (* t 3) 255)
			r (logand (+ 128 (* 127 (sin (* phase 6.28)))) 0xff)
			g (logand (+ 128 (* 127 (sin (+ (* phase 6.28) 2.09)))) 0xff)
			b (logand (+ 128 (* 127 (sin (+ (* phase 6.28) 4.18)))) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-ocean (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (/ t 8) 0xff)
			g (logand (+ 64 (/ (* t 3) 4)) 0xff)
			b (logand (+ 128 (/ t 2)) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-sunset (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (+ 128 (/ t 2)) 0xff)
			g (logand (/ t 2) 0xff)
			b (logand (/ t 4) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-electric (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (* (logand t 1) 255) 0xff)
			g (logand (+ 64 (/ t 2)) 0xff)
			b (logand (+ 128 (* (- 255 t) (/ 1 2))) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-forest (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (/ t 4) 0xff)
			g (logand (+ 64 (/ (* t 5) 4)) 0xff)
			b (logand (/ t 8) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun get-color-copper (iter max_iter)
	(if (>= iter max_iter)
		+argb_black
		(defq t (/ (* iter 255) max_iter)
			r (logand (+ 128 (/ t 2)) 0xff)
			g (logand (+ 64 (/ (* t 3) 4)) 0xff)
			b (logand (/ t 4) 0xff))
		(+ +argb_black (<< r 16) (<< g 8) b)))

(defun apply-color-scheme (iter max_iter scheme)
	(case scheme
		(+color_scheme_classic (get-color-classic iter max_iter))
		(+color_scheme_rainbow (get-color-rainbow iter max_iter))
		(+color_scheme_fire (get-color-fire iter max_iter))
		(+color_scheme_ice (get-color-ice iter max_iter))
		(+color_scheme_psychedelic (get-color-psychedelic iter max_iter))
		(+color_scheme_ocean (get-color-ocean iter max_iter))
		(+color_scheme_sunset (get-color-sunset iter max_iter))
		(+color_scheme_electric (get-color-electric iter max_iter))
		(+color_scheme_forest (get-color-forest iter max_iter))
		(+color_scheme_copper (get-color-copper iter max_iter))
		(:t (get-color-classic iter max_iter))))
