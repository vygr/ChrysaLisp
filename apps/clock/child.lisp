;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(defun-bind make-time ()
	(defq sec (* (/ (time) 1000000) 1.0))
	(setq seconds (fmod sec 60.0) minutes (fmod (/ sec 60) 60.0) hours (fmod (/ sec 3600) 24.0))
	(cat (pad (>> hours fp_shift) 2 "0")
		":" (pad (>> minutes fp_shift) 2 "0")
		":" (pad (>> seconds fp_shift) 2 "0")))

(defun-bind transform (_ a s &optional x y)
	(defq sa (fsin a) ca (fcos a) x (opt x 0) y (opt y 0))
	(points-transform _ _
		(fmul s ca) (fmul s (neg sa)) (fmul s sa) (fmul s ca) (fmul s (+ x 0.5)) (fmul s (+ y 0.5))))

;read args from parent and init globals
(bind '(display clock clock_size clock_scale) (mail-read (task-mailbox)))
(defq hours 0 minutes 0 seconds 0 face (list)
	scale (fmul clock_size clock_scale) eps 0.25)

;create static clock face
(points-stroke-polylines (fmul scale 0.01) eps join_miter cap_butt cap_butt
	(reduce (lambda (l a)
		(push l (transform (points 0.0 0.35 0.0 0.44) (/ (* a fp_2pi) 12) scale))) (range 0 12) (list))
	(points-stroke-polylines (fmul scale 0.03) eps join_miter cap_butt cap_butt
		(reduce (lambda (l a)
			(push l (transform (points 0.0 0.35 0.0 0.44) (* a fp_hpi) scale))) (range 0 4) (list))
		(points-stroke-polygons (fmul scale 0.02) eps join_miter
			(list (points-gen-arc (fmul scale 0.5) (fmul scale 0.5) 0 fp_2pi (fmul scale 0.48) eps (points)))
			face)))

;while not told to quit
(until (mail-poll (array (task-mailbox)))
	(set display 'text (make-time))
	(canvas-fill clock 0)
	(canvas-set-color clock argb_white)
	(canvas-fpoly clock 0.0 0.0 0 (slice 0 1 face))
	(canvas-set-color clock argb_black)
	(canvas-fpoly clock 0.0 0.0 0 face)
	;hour and minute hands
	(defq _ (points-stroke-polylines (const (fmul scale 0.02)) eps join_miter cap_round cap_tri
		(list (transform (points 0.0 0.04 0.0 -0.22) (/ (fmul hours fp_2pi) 12) scale)
			(transform (points 0.0 0.04 0.0 -0.38) (/ (fmul minutes fp_2pi) 60) scale))
		(list)))
	(canvas-set-color clock 0xa0000000)
	(canvas-fpoly clock (const (fmul scale 0.01)) (const (fmul scale 0.01)) 1 _)
	(canvas-set-color clock argb_green)
	(canvas-fpoly clock 0.0 0.0 1 _)
	;second hand
	(setq _ (points-stroke-polylines (const (fmul scale 0.01)) eps join_miter cap_round cap_tri
		(list (transform (points 0.0 0.04 0.0 -0.34) (/ (* (>> seconds fp_shift) fp_2pi) 60) scale))
		(list)))
	(canvas-set-color clock 0xa0000000)
	(canvas-fpoly clock (const (fmul scale 0.01)) (const (fmul scale 0.01)) 0 _)
	(canvas-set-color clock argb_red)
	(canvas-fpoly clock 0.0 0.0 0 _)
	(canvas-swap clock)
	(view-dirty display)
	(task-sleep 1000000))
(mail-read (task-mailbox))
