;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

;read args from parent and init globals
(bind '(display clock clock_size clock_scale) (mail-read (task-mailbox)))
(defq hours 0.0 minutes 0.0 seconds 0.0 two_pi (fixed fp_2pi) half 0.5
	face (list) scale (* clock_size clock_scale) eps 0.25)

(defun-bind make-time ()
	(setq seconds (i2f (/ (time) 1000000))
		minutes (/ seconds 60.0)
		hours (/ minutes 60.0))
	(cat (pad (f2i (% hours 12.0)) 2 "0")
		":" (pad (f2i (% minutes 60.0)) 2 "0")
		":" (pad (f2i (% seconds 60.0)) 2 "0")))

(defun-bind transform (_ a s &optional x y)
	(defq sa (fsin a) ca (fcos a) x (opt x 0.0) y (opt y 0.0))
	(path-transform
		(* s ca) (* s (* sa -1.0))
		(* s sa) (* s ca)
		(* s (+ x half)) (* s (+ y half)) _ _))

(defun-bind main ()
	;create static clock face
	(path-stroke-polygons face (* scale 0.02) eps join_miter
		(list (path-gen-arc (* scale half) (* scale half) 0.0 two_pi (* scale 0.48) eps (path))))
	(path-stroke-polylines face (* scale 0.03) eps join_miter cap_butt cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (* (i2f a) (fixed fp_hpi)) scale))) (range 0 4) (list)))
	(path-stroke-polylines face (* scale 0.01) eps join_miter cap_butt cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (/ (* (i2f a) two_pi) 12.0) scale))) (range 0 12) (list)))

	;while not told to quit
	(until (mail-poll (array (task-mailbox)))
		;clock face
		(set display 'text (make-time))
		(canvas-fill clock 0)
		(canvas-set-color clock argb_white)
		(canvas-fpoly clock 0.0 0.0 0 (slice 0 1 face))
		(canvas-set-color clock argb_black)
		(canvas-fpoly clock 0.0 0.0 0 face)

		;hour and minute hands
		(defq _ (path-stroke-polylines (list) (const (* scale 0.02)) eps join_miter cap_round cap_tri
			(list (transform (path 0.0 0.04 0.0 -0.22) (/ (* hours two_pi) 12.0) scale)
				(transform (path 0.0 0.04 0.0 -0.38) (/ (* minutes two_pi) 60.0) scale))))
		(canvas-set-color clock 0xa0000000)
		(canvas-fpoly clock (const (* scale 0.01)) (const (* scale 0.01)) 1 _)
		(canvas-set-color clock argb_green)
		(canvas-fpoly clock 0.0 0.0 1 _)

		;second hand
		(defq _ (path-stroke-polylines (list) (const (* scale 0.01)) eps join_miter cap_round cap_tri
			(list (transform (path 0.0 0.04 0.0 -0.34) (/ (* (% seconds 60.0) two_pi) 60.0) scale))))
		(canvas-set-color clock 0xa0000000)
		(canvas-fpoly clock (const (* scale 0.01)) (const (* scale 0.01)) 0 _)
		(canvas-set-color clock argb_red)
		(canvas-fpoly clock 0.0 0.0 0 _)

		(canvas-swap clock)
		(view-dirty display)
		(task-sleep 1000000))
	(mail-read (task-mailbox)))
