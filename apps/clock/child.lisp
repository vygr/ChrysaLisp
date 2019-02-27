;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(defun make-time ()
	(defq sec (mul (div (time) 1000000) 1.0))
	(setq seconds (fmod sec 60.0) minutes (fmod (div sec 60) 60.0) hours (fmod (div sec 3600) 24.0))
	(cat (pad (bit-shr hours fp_shift) 2 "0")
		":" (pad (bit-shr minutes fp_shift) 2 "0")
		":" (pad (bit-shr seconds fp_shift) 2 "0")))

(defun transform (_ a s &optional x y)
	(defq sa (fsin a) ca (fcos a) x (opt x 0) y (opt y 0))
	(points-transform _ _
		(fmul s ca) (fmul s (neg sa)) (fmul s sa) (fmul s ca) (fmul s (add x 0.5)) (fmul s (add y 0.5))))

;read args from parent and init globals
(bind '(display clock clock_size clock_scale) (mail-mymail))
(defq hours 0 minutes 0 seconds 0 stack (array) face (list)
	scale (fmul clock_size clock_scale) eps 0.125)

;create static clock face
(points-stroke-polylines stack (fmul scale 0.01) eps join-miter cap-butt cap-butt
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (div (mul a fp_2pi) 12) scale))) (range 0 12) (list))
	(points-stroke-polylines stack (fmul scale 0.03) eps join-miter cap-butt cap-butt
		(reduce (lambda (l a)
			(push l (transform (points 0.0 0.35 0.0 0.44) (mul a fp_hpi) scale))) (range 0 4) (list))
		(points-stroke-polygons stack (fmul scale 0.02) eps join-miter
			(list (points-gen-arc stack (fmul scale 0.5) (fmul scale 0.5) 0 fp_2pi (fmul scale 0.48) eps (points)))
			face)))

;while not told to quit
(until (mail-trymail)
	(set display 'text (make-time))
	(canvas-set-color clock argb_white)
	(canvas-fpoly clock 0.0 0.0 0 (slice 0 1 face))
	(canvas-set-color clock argb_black)
	(canvas-fpoly clock 0.0 0.0 1 face)
	;hour and minute hands
	(defq _ (points-stroke-polylines stack (const (fmul scale 0.02)) eps join-miter cap-round cap-tri
		(list (transform (points 0.0 0.04 0.0 -0.22) (div (fmul hours fp_2pi) 12) scale)
			(transform (points 0.0 0.04 0.0 -0.38) (div (fmul minutes fp_2pi) 60) scale))
		(list)))
	(canvas-set-color clock 0xa0000000)
	(canvas-fpoly clock (const (fmul scale 0.01)) (const (fmul scale 0.01)) 0 _)
	(canvas-set-color clock argb_green)
	(canvas-fpoly clock 0.0 0.0 0 _)
	;second hand
	(setq _ (points-stroke-polylines stack (const (fmul scale 0.01)) eps join-miter cap-round cap-tri
		(list (transform (points 0.0 0.04 0.0 -0.34) (div (mul (bit-shr seconds fp_shift) fp_2pi) 60) scale))
		(list)))
	(canvas-set-color clock 0xa0000000)
	(canvas-fpoly clock (const (fmul scale 0.01)) (const (fmul scale 0.01)) 0 _)
	(canvas-set-color clock argb_red)
	(canvas-fpoly clock 0.0 0.0 0 _)
	(view-dirty display)
	(canvas-swap clock)
	(task-sleep 1000000))
