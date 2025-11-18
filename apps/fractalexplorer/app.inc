;message structure for distributed rendering

(structure +job 0
	(long key)
	(netid reply)
	(long x y x1 y1 w h cx cy z)
	(long fractal_type param1 param2 param3 color_scheme)
	(long smooth_coloring orbit_trap))

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
		lyapunov mandel3 mandel4 mandelbox
		celtic buffalo perpendicular heart magnet))

;color scheme enumeration

(enums +color_scheme 0
	(enum classic rainbow fire ice psychedelic ocean sunset electric forest copper
		smooth_rainbow smooth_fire smooth_ice smooth_ocean smooth_sunset))

;orbit trap types

(enums +orbit_trap 0
	(enum none point line circle cross square))

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

;smooth coloring helpers (for continuous iteration count)

(defun get-color-smooth-rainbow (smooth_iter)
	(defq t (logand (>>> smooth_iter 5) 0xff)
		r (logand (+ 128 (* 127 (abs (- (/ (* t 6) 255) 3)))) 0xff)
		g (logand (+ 128 (* 127 (abs (- (/ (* (+ t 128) 6) 255) 3)))) 0xff)
		b (logand (+ 128 (* 127 (abs (- (/ (* (+ t 192) 6) 255) 3)))) 0xff))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun get-color-smooth-fire (smooth_iter)
	(defq t (logand (>>> smooth_iter 5) 0xff)
		r (min 255 (* t 2))
		g (logand (/ (* t 3) 2) 0xff)
		b (logand (/ t 4) 0xff))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun get-color-smooth-ice (smooth_iter)
	(defq t (logand (>>> smooth_iter 5) 0xff)
		r (logand (/ t 4) 0xff)
		g (logand (/ (* t 3) 2) 0xff)
		b (min 255 (* t 2)))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun get-color-smooth-ocean (smooth_iter)
	(defq t (logand (>>> smooth_iter 5) 0xff)
		r (logand (/ t 8) 0xff)
		g (logand (+ 64 (/ (* t 3) 4)) 0xff)
		b (logand (+ 128 (/ t 2)) 0xff))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun get-color-smooth-sunset (smooth_iter)
	(defq t (logand (>>> smooth_iter 5) 0xff)
		r (logand (+ 128 (/ t 2)) 0xff)
		g (logand (/ t 2) 0xff)
		b (logand (/ t 4) 0xff))
	(+ +argb_black (<< r 16) (<< g 8) b))

;orbit trap coloring

(defun apply-orbit-trap-color (trap_dist trap_type scheme)
	(defq intensity (min 255 (logand (>>> trap_dist 21) 0xff)))
	(case scheme
		(+color_scheme_fire
			(+ +argb_black (<< intensity 16) (<< (/ intensity 2) 8)))
		(+color_scheme_ice
			(+ +argb_black (<< (/ intensity 4) 16) (<< (/ intensity 2) 8) intensity))
		(+color_scheme_rainbow
			(defq phase (logand (>>> trap_dist 23) 0xff)
				r (logand (+ 128 (* 127 (abs (- (/ (* phase 6) 255) 3)))) 0xff)
				g (logand (+ 128 (* 127 (abs (- (/ (* (+ phase 128) 6) 255) 3)))) 0xff)
				b (logand (+ 128 (* 127 (abs (- (/ (* (+ phase 192) 6) 255) 3)))) 0xff))
			(+ +argb_black (<< r 16) (<< g 8) b))
		(:t
			(+ +argb_black (<< intensity 16) (<< intensity 8) intensity))))

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
		(+color_scheme_smooth_rainbow (get-color-smooth-rainbow iter))
		(+color_scheme_smooth_fire (get-color-smooth-fire iter))
		(+color_scheme_smooth_ice (get-color-smooth-ice iter))
		(+color_scheme_smooth_ocean (get-color-smooth-ocean iter))
		(+color_scheme_smooth_sunset (get-color-smooth-sunset iter))
		(:t (get-color-classic iter max_iter))))

;bookmark and preset structures

(structure +bookmark 0
	(long fractal_type)
	(long cx cy zoom)
	(long param1 param2 param3)
	(long color_scheme)
	(offset name 128))

;famous Julia set presets (real, imag)
(defq +julia_presets (list
	(list "Dendrite" (mbfp-from-fixed -0.4) (mbfp-from-fixed 0.6))
	(list "San Marco" (mbfp-from-fixed -0.75) (mbfp-from-fixed 0.11))
	(list "Siegel Disk" (mbfp-from-fixed -0.391) (mbfp-from-fixed -0.587))
	(list "Dragon" (mbfp-from-fixed -0.8) (mbfp-from-fixed 0.156))
	(list "Spiral" (mbfp-from-fixed -0.7269) (mbfp-from-fixed 0.1889))
	(list "Douady Rabbit" (mbfp-from-fixed -0.123) (mbfp-from-fixed 0.745))
	(list "Airplane" (mbfp-from-fixed -0.5251993) (mbfp-from-fixed 0.5251993))
	(list "Cauliflower" (mbfp-from-fixed 0.25) (mbfp-from-fixed 0.0))))
