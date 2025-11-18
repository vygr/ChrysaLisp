;jit compile apps native functions
(jit "apps/fractalexplorer/" "lisp.vp" '("julia_depth" "burning_ship_depth" "newton_depth"))

(import "./app.inc")

(enums +select 0
	(enum main timeout))

;Julia set iteration
(defun julia_depth (x0 y0 cr ci)
	(defq i -1 xc x0 yc y0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) ci)
			xc (+ (- x2 y2) cr)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Burning Ship fractal iteration
(defun burning_ship_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq xc (abs xc) yc (abs yc)
			yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0)
			xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Newton fractal - finds which root z^3-1=0
(defun newton_depth (x0 y0)
	(defq i -1 zr x0 zi y0 max_iter 64)
	(while (and (/= (++ i) max_iter))
		;z^3
		(defq zr2 (mbfp-mul zr zr) zi2 (mbfp-mul zi zi))
		(defq zr3 (mbfp-mul zr (- zr2 (mbfp-mul (mbfp-from-fixed 3.0) zi2))))
		(defq zi3 (mbfp-mul zi (- (mbfp-mul (mbfp-from-fixed 3.0) zr2) zi2)))

		;z^3 - 1
		(setq zr3 (- zr3 (mbfp-from-fixed 1.0)))

		;3*z^2
		(defq dzr (mbfp-mul (mbfp-from-fixed 3.0) (- zr2 zi2)))
		(defq dzi (mbfp-mul (mbfp-from-fixed 6.0) zr zi))

		;division: (z^3-1)/(3*z^2)
		(defq denom (+ (mbfp-mul dzr dzr) (mbfp-mul dzi dzi)))
		(when (> denom (mbfp-from-fixed 0.0001))
			(defq new_zr (- zr (/ (+ (mbfp-mul zr3 dzr) (mbfp-mul zi3 dzi)) denom)))
			(defq new_zi (- zi (/ (- (mbfp-mul zi3 dzr) (mbfp-mul zr3 dzi)) denom)))

			;check convergence
			(when (< (+ (mbfp-mul (- new_zr zr) (- new_zr zr))
					   (mbfp-mul (- new_zi zi) (- new_zi zi)))
				   (mbfp-from-fixed 0.0001))
				(setq i max_iter))

			(setq zr new_zr zi new_zi)))
	i)

;Tricorn (Mandelbar) iteration
(defun tricorn_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (- y0 (mbfp-mul (mbfp-from-fixed 2.0) xc yc))
			xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Phoenix fractal iteration
(defun phoenix_depth (x0 y0 pr pi)
	(defq i -1 xc 0 yc 0 x2 0 y2 0 prev_x 0 prev_y 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(defq new_x (+ (- x2 y2) (mbfp-mul pr prev_x) x0))
		(defq new_y (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) (mbfp-mul pi prev_y) y0))
		(setq prev_x xc prev_y yc xc new_x yc new_y
			x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc)))
	i)

;Mandelbrot power 3
(defun mandel3_depth (x0 y0)
	(defq i -1 xc 0 yc 0)
	(while (and (/= (++ i) 255) (< (+ (mbfp-mul xc xc) (mbfp-mul yc yc)) (mbfp-from-fixed 4.0)))
		(defq x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc))
		(defq new_x (+ (mbfp-mul xc (- x2 (mbfp-mul (mbfp-from-fixed 3.0) y2))) x0))
		(defq new_y (+ (mbfp-mul yc (- (mbfp-mul (mbfp-from-fixed 3.0) x2) y2)) y0))
		(setq xc new_x yc new_y))
	i)

;Mandelbrot power 4
(defun mandel4_depth (x0 y0)
	(defq i -1 xc 0 yc 0)
	(while (and (/= (++ i) 255) (< (+ (mbfp-mul xc xc) (mbfp-mul yc yc)) (mbfp-from-fixed 4.0)))
		(defq x2 (mbfp-mul xc xc) y2 (mbfp-mul yc yc))
		(defq x4 (mbfp-mul x2 x2) y4 (mbfp-mul y2 y2))
		(defq new_x (+ (- (+ x4 y4) (mbfp-mul (mbfp-from-fixed 6.0) x2 y2)) x0))
		(defq new_y (+ (mbfp-mul (mbfp-from-fixed 4.0) xc yc (- x2 y2)) y0))
		(setq xc new_x yc new_y))
	i)

;Mandelbox approximation
(defun mandelbox_depth (x0 y0 scale)
	(defq i -1 vx x0 vy y0 max_iter 128)
	(while (and (/= (++ i) max_iter) (< (+ (mbfp-mul vx vx) (mbfp-mul vy vy)) (mbfp-from-fixed 256.0)))
		;box fold
		(when (> vx (mbfp-from-fixed 1.0)) (setq vx (- (mbfp-from-fixed 2.0) vx)))
		(when (< vx (mbfp-from-fixed -1.0)) (setq vx (- (mbfp-from-fixed -2.0) vx)))
		(when (> vy (mbfp-from-fixed 1.0)) (setq vy (- (mbfp-from-fixed 2.0) vy)))
		(when (< vy (mbfp-from-fixed -1.0)) (setq vy (- (mbfp-from-fixed -2.0) vy)))

		;sphere fold
		(defq r2 (+ (mbfp-mul vx vx) (mbfp-mul vy vy)))
		(cond
			((< r2 (mbfp-from-fixed 0.25))
				(setq vx (mbfp-mul vx (mbfp-from-fixed 4.0))
					vy (mbfp-mul vy (mbfp-from-fixed 4.0))))
			((< r2 (mbfp-from-fixed 1.0))
				(setq vx (/ vx r2) vy (/ vy r2))))

		;scale and translate
		(setq vx (+ (mbfp-mul vx scale) x0)
			vy (+ (mbfp-mul vy scale) y0)))
	i)

;Lyapunov fractal approximation (simplified)
(defun lyapunov_depth (x0 y0)
	(defq i -1 a (+ (mbfp-from-fixed 2.5) (mbfp-mul x0 (mbfp-from-fixed 1.5)))
		b (+ (mbfp-from-fixed 2.5) (mbfp-mul y0 (mbfp-from-fixed 1.5)))
		x (mbfp-from-fixed 0.5) sum 0 max_iter 64)
	(while (and (/= (++ i) max_iter))
		(defq r (if (= (logand i 1) 0) a b))
		(setq x (mbfp-mul r x (- (mbfp-from-fixed 1.0) x)))
		(when (> x 0)
			(setq sum (+ sum (if (> x 0) 1 -1)))))
	(logand (abs (/ sum 2)) 0xff))

;Celtic Mandelbrot
(defun celtic_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0)
			xc (+ (- (abs x2) y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Buffalo fractal
(defun buffalo_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) (abs xc) (abs yc)) y0)
			xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Perpendicular Mandelbrot
(defun perpendicular_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 4.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc (abs yc)) y0)
			xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Heart fractal
(defun heart_depth (x0 y0)
	(defq i -1 xc 0 yc 0 x2 0 y2 0)
	(while (and (/= (++ i) 255) (< (+ x2 y2) (mbfp-from-fixed 16.0)))
		(setq yc (+ (mbfp-mul (mbfp-from-fixed 2.0) xc yc) y0)
			xc (+ (- x2 y2) x0)
			x2 (mbfp-mul xc xc)
			y2 (mbfp-mul yc yc)))
	i)

;Magnet Type 1
(defun magnet_depth (x0 y0)
	(defq i -1 zr x0 zi y0 max_iter 128)
	(while (and (/= (++ i) max_iter))
		;z^2 + c - 1
		(defq zr2 (mbfp-mul zr zr) zi2 (mbfp-mul zi zi))
		(defq nr (+ (- zr2 zi2) x0 (mbfp-from-fixed -1.0)))
		(defq ni (+ (mbfp-mul (mbfp-from-fixed 2.0) zr zi) y0))

		;2z + c - 2
		(defq dr (+ (mbfp-mul (mbfp-from-fixed 2.0) zr) x0 (mbfp-from-fixed -2.0)))
		(defq di (+ (mbfp-mul (mbfp-from-fixed 2.0) zi) y0))

		;division
		(defq denom (+ (mbfp-mul dr dr) (mbfp-mul di di)))
		(when (> denom (mbfp-from-fixed 0.0001))
			(setq zr (/ (+ (mbfp-mul nr dr) (mbfp-mul ni di)) denom))
			(setq zi (/ (- (mbfp-mul ni dr) (mbfp-mul nr di)) denom))

			;check escape
			(when (> (+ (mbfp-mul zr zr) (mbfp-mul zi zi)) (mbfp-from-fixed 100.0))
				(setq i max_iter))))
	(min 254 i))

;native versions
(ffi "apps/fractalexplorer/julia_depth" julia_depth)
(ffi "apps/fractalexplorer/burning_ship_depth" burning_ship_depth)
(ffi "apps/fractalexplorer/newton_depth" newton_depth)

;main fractal renderer
(defun render_fractal (key mbox x y x1 y1 w h cx cy z ftype p1 p2 p3 cscheme smooth orbit)
	(write-int (defq reply (string-stream (cat ""))) (list x y x1 y1))
	(-- y)
	(while (/= (++ y) y1)
		(defq xp (dec x))
		(while (/= (++ xp) x1)
			(defq px (+ (mbfp-offset xp w z) cx)
				py (+ (mbfp-offset y h z) cy)
				iter 0)

			;compute iteration count based on fractal type
			(case ftype
				(+fractal_julia
					(setq iter (julia_depth px py p1 p2)))
				(+fractal_burning_ship
					(setq iter (burning_ship_depth px py)))
				(+fractal_newton
					(setq iter (newton_depth px py)))
				(+fractal_tricorn
					(setq iter (tricorn_depth px py)))
				(+fractal_phoenix
					(setq iter (phoenix_depth px py p1 p2)))
				(+fractal_mandel3
					(setq iter (mandel3_depth px py)))
				(+fractal_mandel4
					(setq iter (mandel4_depth px py)))
				(+fractal_mandelbox
					(setq iter (mandelbox_depth px py p1)))
				(+fractal_lyapunov
					(setq iter (lyapunov_depth px py)))
				(+fractal_celtic
					(setq iter (celtic_depth px py)))
				(+fractal_buffalo
					(setq iter (buffalo_depth px py)))
				(+fractal_perpendicular
					(setq iter (perpendicular_depth px py)))
				(+fractal_heart
					(setq iter (heart_depth px py)))
				(+fractal_magnet
					(setq iter (magnet_depth px py)))
				(:t (setq iter 0)))

			(write-char reply iter))
		(task-slice))
	(write-long reply key)
	(mail-send mbox (str reply)))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				(setq running :nil))
			((= idx +select_main)
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(defq key (getf msg +job_key)
					mbox (getf msg +job_reply)
					x (getf msg +job_x)
					y (getf msg +job_y)
					x1 (getf msg +job_x1)
					y1 (getf msg +job_y1)
					w (getf msg +job_w)
					h (getf msg +job_h)
					cx (getf msg +job_cx)
					cy (getf msg +job_cy)
					z (getf msg +job_z)
					ftype (getf msg +job_fractal_type)
					p1 (getf msg +job_param1)
					p2 (getf msg +job_param2)
					p3 (getf msg +job_param3)
					cscheme (getf msg +job_color_scheme)
					smooth (getf msg +job_smooth_coloring)
					orbit (getf msg +job_orbit_trap))
				(render_fractal key mbox x y x1 y1 w h cx cy z ftype p1 p2 p3 cscheme smooth orbit)))))
