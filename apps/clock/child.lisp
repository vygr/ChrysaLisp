;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(defun make-time ()
	(defq sec (div (time) 1000000))
	(setq seconds (mod sec 60)
		minutes (fmod (fdiv sec 60.0) 60.0)
		hours (fmod (fdiv sec 60.0 60.0) 24.0))
	(cat (pad (bit-shr hours fp_shift) 2 "0")
		":" (pad (bit-shr minutes fp_shift) 2 "0")
		":" (pad seconds 2 "0")))

(defun transform (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(points-transform _ _
		(fmul clock_size clock_scale ca) (fmul clock_size clock_scale (neg sa))
		(fmul clock_size clock_scale sa) (fmul clock_size clock_scale ca)
		(fmul clock_size clock_scale 0.5) (fmul clock_size clock_scale 0.5)))

;read args from parent and init globals
(bind '(display clock clock_size clock_scale) (mail-mymail))
(defq hours 0 minutes 0 seconds 0 stack (array) face (list) eps 0.125)

;create static clock face
(points-stroke-polygons face stack
	(list (points-gen-arc (points) stack
		(fmul clock_size clock_scale 0.5)
		(fmul clock_size clock_scale 0.5)
		0 fp_2pi
		(fmul clock_size clock_scale 0.48) eps))
	join-miter
	(fmul clock_size clock_scale 0.02) eps)
(points-stroke-polylines face stack
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (mul a fp_hpi))))
			(range 0 4) (list))
	join-miter cap-butt cap-butt
	(fmul clock_size clock_scale 0.03) eps)
(points-stroke-polylines face stack
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (mul a (const (div fp_2pi 12))))))
			(range 0 12) (list))
	join-miter cap-butt cap-butt
	(fmul clock_size clock_scale 0.01) eps)

;while not told to quit
(until (mail-trymail)
	(set display 'text (make-time))
	(canvas-fill clock 0)
	(canvas-set-fpoly clock (slice 0 1 face) argb_white 0)
	(canvas-set-fpoly clock face argb_black 1)
	;hour and miniute hands
	(canvas-set-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.04 0.0 -0.22) (fmul hours (const (div fp_2pi 12))))
				(transform (points 0.0 0.04 0.0 -0.40) (fmul minutes (const (div fp_2pi 60)))))
			join-miter cap-round cap-tri
			(fmul clock_size clock_scale 0.02) eps) argb_black 0)
	;second hand
	(canvas-set-fpoly clock
		(points-stroke-polylines (list) stack
			(list (transform (points 0.0 0.05 0.0 -0.40) (mul seconds (const (div fp_2pi 60)))))
			join-miter cap-round cap-tri
			(fmul clock_size clock_scale 0.01) eps) argb_red 0)
	(view-dirty display)
	(canvas-swap clock)
	(task-sleep 1000000))
