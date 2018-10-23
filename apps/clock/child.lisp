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
(points-stroke-polygons face stack
	(list (points-gen-arc (points) stack (fmul scale 0.5) (fmul scale 0.5) 0 fp_2pi (fmul scale 0.48) eps))
	join-miter (fmul scale 0.02) eps)
(points-stroke-polylines face stack
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (mul a fp_hpi) scale))) (range 0 4) (list))
	join-miter cap-butt cap-butt (fmul scale 0.03) eps)
(points-stroke-polylines face stack
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (div (mul a fp_2pi) 12) scale))) (range 0 12) (list))
	join-miter cap-butt cap-butt (fmul scale 0.01) eps)

;while not told to quit
(until (mail-trymail)
	(set display 'text (make-time))
	(canvas-fill clock 0)
	(canvas-set-fpoly clock (slice 0 1 face) argb_white 0)
	(canvas-set-fpoly clock face argb_black 1)
	;hour and minute hands
	(canvas-blend-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.04 0.0 -0.22) (div (fmul hours fp_2pi) 12) scale 0.01 0.01)
				(transform (points 0.0 0.04 0.0 -0.38) (div (fmul minutes fp_2pi) 60) scale 0.01 0.01))
			join-miter cap-round cap-tri (const (fmul scale 0.02)) eps) 0x80000000 0)
	(canvas-set-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.04 0.0 -0.22) (div (fmul hours fp_2pi) 12) scale)
				(transform (points 0.0 0.04 0.0 -0.38) (div (fmul minutes fp_2pi) 60) scale))
			join-miter cap-round cap-tri (const (fmul scale 0.02)) eps) argb_green 0)
	;second hand
	(canvas-blend-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.04 0.0 -0.34) (div (mul (bit-shr seconds fp_shift) fp_2pi) 60) scale 0.01 0.01))
			join-miter cap-round cap-tri (const (fmul scale 0.01)) eps) 0x80000000 0)
	(canvas-set-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.04 0.0 -0.34) (div (mul (bit-shr seconds fp_shift) fp_2pi) 60) scale))
			join-miter cap-round cap-tri (const (fmul scale 0.01)) eps) argb_red 0)
	(view-dirty display)
	(canvas-swap clock)
	(task-sleep 1000000))
