;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'lib/math/math.inc)
(import 'lib/date/date.inc)

;read args from parent. display and clock can both be t or one be nil.
(bind '(display clock scale) (mail-read (task-mailbox)))
;creates local_timezone
(timezone-init *env_clock_timezone*)

(defq seconds 0.0 second 0 minutes 0.0 minute 0 hours 0.0 hour 0 dotw (str) face (list) eps 0.25)

(defun-bind make-time ()
	(bind '(s m h _ _ _ w) (date))
	(setq second s seconds (i2f s) minute m minutes (i2f m) hour h hours (i2f hours) dotw w))

(defun-bind view-digital-time ()
	(if (and *env_clock_twelve_hour* (> hour 12)) (setq hour (- hour 12)))
	(cat 	(if *env_clock_dotw* (day-of-the-week dotw) "") " " 
		(if *env_clock_pad_hour* (pad hour 2 "0") (str hour)) (str ":" (pad minute 2 "0"))
		(if (eql *env_clock_seconds* t) (str ":" (pad second 2 "0")) "")))

(defun-bind transform (_ a s &optional x y)
	(defq sa (sin a) ca (cos a) x (opt x 0.0) y (opt y 0.0))
	(path-transform
		(* s ca) (* s (* sa -1.0))
		(* s sa) (* s ca)
		(* s (+ x 0.5)) (* s (+ y 0.5)) _ _))

(defun-bind create-clockface ()
	;create static clock face
	(path-stroke-polygons face (* scale 0.02) eps join_miter
		(list (path-gen-arc (* scale 0.5) (* scale 0.5) 0.0 (const fp_2pi) (* scale 0.48) eps (path))))
	(path-stroke-polylines face (* scale 0.03) eps join_miter cap_butt cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (* (i2f a) (const fp_hpi)) scale))) (range 0 4) (list)))
	(path-stroke-polylines face (* scale 0.01) eps join_miter cap_butt cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (/ (* (i2f a) (const fp_2pi)) 12.0) scale))) (range 0 12) (list))))

(defun-bind view-analog-time ()
		(canvas-fill clock 0)
		(canvas-set-color clock argb_white)
		(canvas-fpoly clock 0.0 0.0 0 (slice 0 1 face))
		(canvas-set-color clock argb_black)
		(canvas-fpoly clock 0.0 0.0 0 face)

		;hour and minute hands
		(defq _ (path-stroke-polylines (list) (const (* scale 0.02)) eps join_miter cap_round cap_tri
			(list (transform (path 0.0 0.04 0.0 -0.22) (/ (* hours (const fp_2pi)) 12.0) scale)
				(transform (path 0.0 0.04 0.0 -0.38) (/ (* minutes (const fp_2pi)) 60.0) scale))))
		(canvas-set-color clock 0xa0000000)
		(canvas-fpoly clock (const (* scale 0.01)) (const (* scale 0.01)) 1 _)
		(canvas-set-color clock argb_green)
		(canvas-fpoly clock 0.0 0.0 1 _)

		;second hand
		(defq _ (path-stroke-polylines (list) (const (* scale 0.01)) eps join_miter cap_round cap_tri
			(list (transform (path 0.0 0.04 0.0 -0.34) (/ (* (% seconds 60.0) (const fp_2pi)) 60.0) scale))))
		(canvas-set-color clock 0xa0000000)
		(canvas-fpoly clock (const (* scale 0.01)) (const (* scale 0.01)) 0 _)
		(canvas-set-color clock argb_red)
		(canvas-fpoly clock 0.0 0.0 0 _))

(defun-bind main ()
	(defq offset (timezone-lookup *env_clock_timezone*))
	(make-time)
	(if clock
			(create-clockface))
	(when display 
			(set display :text (view-digital-time))
			;prevents clipping the label
			(view-layout display))
	;while not told to quit
	(until (mail-poll (array (task-mailbox)))
		(make-time)
			(when clock
				(view-analog-time)
				(canvas-swap clock))
			(when display
				(set display :text (view-digital-time))
				(view-dirty display))
		;keeps the current time in sync with given time value.
		(task-sleep 50000))
	(mail-read (task-mailbox)))
