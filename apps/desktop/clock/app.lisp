(defq *env_clock_analog* :t
	*env_clock_digital* :t
	*env_clock_seconds* :t
	*env_clock_twelve_hour* :nil
	*env_clock_pad_hour* :t
	*env_clock_dotw* :t
	*env_clock_numerals* :t
	*env_clock_timezone* "UTC")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/date/date.inc")

(if (and (not *env_clock_analog*) (not *env_clock_digital*))
	(setq *env_clock_analog* :t))

(enums +event 0
	(enum close))

(enums +select 0
	(enum main timer))

(defq clock_size 300
	clock_scale 1
	clock_dial (list)
	clock_face (list))

(ui-window *window* ()
	(ui-title-bar _ "Clock" (0xea19) +event_close)
	(if (eql *env_clock_analog* :t)
		(ui-canvas clock clock_size clock_size clock_scale)
		(defq clock :nil))
	(if (eql *env_clock_digital* :t)
		(ui-label *display* (:text "Mon 00:00:00 AM"
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter)
			:font (create-font "fonts/Hack-Regular.ctf" 24)))
		(defq *display* :nil)))

(defun view-digital-time ((sc mn hr & & & wk))
	(defq h hr ampm "")
	(when *env_clock_twelve_hour*
		(setq ampm (if (>= h 12) " PM" " AM")
			h (cond ((= h 0) 12) ((> h 12) (- h 12)) (h))))
	(cat (if *env_clock_dotw* (cat (day-of-the-week wk) " ") "")
		(if *env_clock_pad_hour* (pad h 2 "0") (str h))
		":" (pad mn 2 "0")
		(if *env_clock_seconds* (cat ":" (pad sc 2 "0")) "")
		ampm))

(defun transform (%0 a s &optional x y)
	(defq sa (sin a) ca (cos a) x (ifn x 0.0) y (ifn y 0.0))
	(path-transform (fixeds
		(* s ca) (* s (* sa -1.0)) (* s (+ x 0.5))
		(* s sa) (* s ca) (* s (+ y 0.5))) %0 %0))

(defun create-clockface (scale)
	(clear clock_dial clock_face)
	(defq cx (* scale 0.5) cy (* scale 0.5))
	; Dial face disc
	(push clock_dial (path-gen-arc cx cy 0.0 +fp_2pi (* scale 0.445) (path)))
	; Outer rim
	(path-stroke-polygons clock_face (* scale 0.018) +join_miter (list (first clock_dial)))
	; 60 minute/second sub-ticks (0..59)
	(path-stroke-polylines clock_face (* scale 0.005) +join_miter +cap_butt +cap_butt
		(map (lambda (a) (transform (path 0.0 -0.405 0.0 -0.425) (/ (* (n2f a) +fp_2pi) 60.0) scale)) (range 0 60)))
	; 12 hour ticks (0..11)
	(path-stroke-polylines clock_face (* scale 0.012) +join_miter +cap_butt +cap_butt
		(map (lambda (a) (transform (path 0.0 -0.385 0.0 -0.425) (/ (* (n2f a) +fp_2pi) 12.0) scale)) (range 0 12)))
	; 4 major cardinal ticks (12, 3, 6, 9)
	(path-stroke-polylines clock_face (* scale 0.018) +join_miter +cap_butt +cap_butt
		(map (lambda (a) (transform (path 0.0 -0.370 0.0 -0.425) (/ (* (n2f a) +fp_2pi) 4.0) scale)) (range 0 4)))
	; 12 dial numeral glyph paths (1..12)
	(when *env_clock_numerals*
		(defq font_size (max 10 (n2i (* scale 0.062)))
			font (or (create-font "fonts/OpenSans-Bold.ctf" font_size)
					(create-font "fonts/OpenSans-Regular.ctf" font_size)
					(create-font "fonts/Hack-Regular.ctf" font_size)))
		(when font
			(defq num_r (* scale 0.30))
			(each (lambda (n)
				(defq a (/ (* (n2f n) +fp_2pi) 12.0)
					text (str n)
					nx (+ cx (* num_r (sin a)))
					ny (- cy (* num_r (cos a))))
				(bind '(w h) (font-glyph-bounds font text))
				(defq tx (+ nx (* (n2f w) -1.0))
					ty (+ ny (* (n2f h) 0.5))
					mat (fixeds 1.0 0.0 tx 0.0 1.0 ty))
				(each (lambda (p)
					(push clock_face (path-transform mat p p)))
					(font-glyph-paths font text)))
				(range 1 13)))))

(defun view-analog-time (canvas (s m h) scale)
	; Dial disc and face markings
	(.-> canvas
		(:fill 0)
		(:set_color +argb_white)
		(:fpoly 0.0 0.0 +winding_none_zero clock_dial)
		(:set_color +argb_black)
		(:fpoly 0.0 0.0 +winding_none_zero clock_face))
	; Hour and minute hands
	(defq hr_path (transform (path 0.0 0.025 0.0 -0.19) (/ (* h +fp_2pi) 12.0) scale)
		mn_path (transform (path 0.0 0.035 0.0 -0.33) (/ (* m +fp_2pi) 60.0) scale)
		hands (cat
			(path-stroke-polylines (list) (* scale 0.022) +join_miter +cap_round +cap_tri (list hr_path))
			(path-stroke-polylines (list) (* scale 0.015) +join_miter +cap_round +cap_tri (list mn_path))))
	(.-> canvas
		(:set_color 0x50000000)
		(:fpoly (* scale 0.008) (* scale 0.008) +winding_none_zero hands)
		(:set_color +argb_green6)
		(:fpoly 0.0 0.0 +winding_none_zero hands))
	; Second hand
	(when *env_clock_seconds*
		(defq sec_path (transform (path 0.0 0.05 0.0 -0.39) (/ (* (% s 60.0) +fp_2pi) 60.0) scale)
			sec_hand (path-stroke-polylines (list) (* scale 0.007) +join_miter +cap_round +cap_round (list sec_path)))
		(.-> canvas
			(:set_color 0x50000000)
			(:fpoly (* scale 0.008) (* scale 0.008) +winding_none_zero sec_hand)
			(:set_color +argb_red)
			(:fpoly 0.0 0.0 +winding_none_zero sec_hand)))
	; Center hub / cap
	(defq hub (path-gen-arc (* scale 0.5) (* scale 0.5) 0.0 +fp_2pi (* scale 0.022) (path)))
	(.-> canvas
		(:set_color +argb_black)
		(:fpoly 0.0 0.0 +winding_none_zero (list hub))))

(defun next-sec-delay ()
	; Microseconds remaining until the top of the next second
	(- 1000000 (% (pii-time) 1000000)))

(defun main ()
	(timezone-init *env_clock_timezone*)
	(defq select (task-mboxes +select_size) *running* :t)
	(when clock
		(.-> clock (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
		(create-clockface (* (n2f clock_size) (n2f clock_scale)))
		(view-analog-time clock (float-time) (* (n2f clock_size) (n2f clock_scale)))
		(. clock :swap +pixmap_mode_normal))
	(when *display*
		(set *display* :text (view-digital-time (date))))
	(bind '(w h) (. *window* :pref_size))
	(gui-add-front-rpc (. *window* :change 0 0 w h))
	(mail-timeout (elem-get select +select_timer) (next-sec-delay) 0)
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_main)
				(if (= (getf msg +ev_msg_target_id) +event_close)
					(setq *running* :nil)
					(. *window* :event msg)))
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) (next-sec-delay) 0)
				(when clock
					(view-analog-time clock (float-time) (* (n2f clock_size) (n2f clock_scale)))
					(. clock :swap +pixmap_mode_normal))
				(when *display*
					(set *display* :text (view-digital-time (date)))
					(.-> *display* :layout :dirty)))
			(:t (. *window* :event msg))))
	(gui-sub-rpc *window*))
