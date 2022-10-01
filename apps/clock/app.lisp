(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/date/date.inc")

(enums +event 0
	(enum close))

(enums +select 0
	(enum main timer))

(defq clock_size 256 clock_scale 1 dotw :nil face (list) eps 0.25
	seconds 0.0 second 0 minutes 0.0 minute 0 hours 0.0 hour 0 id :t
	rate (/ 1000000 1))

(ui-window *window* ()
	(ui-title-bar _ "Clock" (0xea19) +event_close)
	(if (eql *env_clock_analog* :t)
		(ui-canvas clock clock_size clock_size clock_scale)
		(defq clock :nil))
	(if (eql *env_clock_digital* :t)
		(ui-label display (:text "xxx hh:mm:ss"
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter)
			:font (create-font "fonts/Hack-Regular.ctf" 44)))
		(defq display :nil)))

(defun make-digital-time ()
	(bind '(sc mn hr _ _ _ wk) (date))
	(setq second sc minute mn hour hr dotw wk))

(defun make-analog-time ()
	(bind '(s m h) (float-time))
	(setq seconds s minutes m hours h))

(defun view-digital-time ()
	(if (and *env_clock_twelve_hour* (> hour 12)) (setq hour (- hour 12)))
	(cat (if *env_clock_dotw* (day-of-the-week dotw) "") " " 
		(if *env_clock_pad_hour* (pad hour 2 "0") (str hour)) (str ":" (pad minute 2 "0"))
		(if (eql *env_clock_seconds* :t) (str ":" (pad second 2 "0")) "")))

(defun transform (_ a s &optional x y)
	(defq sa (sin a) ca (cos a) x (opt x 0.0) y (opt y 0.0))
	(path-transform
		(* s ca) (* s (* sa -1.0))
		(* s sa) (* s ca)
		(* s (+ x 0.5)) (* s (+ y 0.5)) _ _))

(defun create-clockface (scale)
	;create static clock face
	(path-stroke-polygons face (* scale 0.02) eps +join_miter
		(list (path-gen-arc (* scale 0.5) (* scale 0.5) 0.0 +fp_2pi (* scale 0.48) eps (path))))
	(path-stroke-polylines face (* scale 0.03) eps +join_miter +cap_butt +cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (* (n2f a) +fp_hpi) scale))) (range 0 4) (list)))
	(path-stroke-polylines face (* scale 0.01) eps +join_miter +cap_butt +cap_butt
		(reduce (lambda (l a)
			(push l (transform (path 0.0 0.35 0.0 0.44) (/ (* (n2f a) +fp_2pi) 12.0) scale))) (range 0 12) (list))))

(defun view-analog-time (scale)
	(.-> clock
		(:fill 0)
		(:set_color +argb_white)
		(:fpoly 0.0 0.0 +winding_odd_even (slice 0 1 face))
		(:set_color +argb_black)
		(:fpoly 0.0 0.0 +winding_odd_even face))
	;hour and minute hands
	(defq _ (path-stroke-polylines (list) (* scale 0.02) eps +join_miter +cap_round +cap_tri
		(list (transform (path 0.0 0.04 0.0 -0.22) (/ (* hours +fp_2pi) 12.0) scale)
			(transform (path 0.0 0.04 0.0 -0.33) (/ (* minutes +fp_2pi) 60.0) scale))))
	(.-> clock
		(:set_color 0xa0000000)
		(:fpoly (* scale 0.01) (* scale 0.01) +winding_none_zero _)
		(:set_color +argb_green)
		(:fpoly 0.0 0.0 +winding_none_zero _))
	;second hand
	(defq _ (path-stroke-polylines (list) (* scale 0.01) eps +join_miter +cap_round +cap_tri
		(list (transform (path 0.0 0.04 0.0 -0.38) (/ (* (% seconds 60.0) +fp_2pi) 60.0) scale))))
	(.-> clock
		(:set_color 0xa0000000)
		(:fpoly (* scale 0.01) (* scale 0.01) +winding_odd_even _)
		(:set_color +argb_red)
		(:fpoly 0.0 0.0 +winding_odd_even _)))

(defun main ()
	;creates local_timezone
	(timezone-init *env_clock_timezone*)
	(defq select (alloc-select +select_size))
	(when clock
		(.-> clock (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
		(create-clockface (* (n2f clock_size) (n2f clock_scale))))
	(bind '(w h) (. *window* :pref_size))
	(gui-add-front (. *window* :change 0 0 w h))
	(mail-timeout (elem-get +select_timer select) 1 0)
	(while id
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main)
				;main mailbox
				(cond
					((= (setq id (getf msg +ev_msg_target_id)) +event_close)
						(setq id :nil))
					(:t (. *window* :event msg))))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get +select_timer select) rate 0)
				(when clock
					(make-analog-time)
					(view-analog-time (* (n2f clock_size) (n2f clock_scale)))
					(. clock :swap))
				(when display
					(make-digital-time)
					(set display :text (view-digital-time))
					(.-> display :layout :dirty)))))
	(free-select select)
	(gui-sub *window*))
