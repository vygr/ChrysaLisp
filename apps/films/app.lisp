(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

(enums +select 0
	(enum main timer tip))

(enums +event 0
	(enum close)
	(enum prev next))

(defq films (files-all "apps/films/data" '(".flm")) index 0 canvas :nil id :t
	+rate (/ 1000000 30))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (0xea19) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll *image_scroll* +scroll_flag_both))

(defun win-refresh (_)
	(defq file (elem-get films (setq index _)))
	(bind '(w h) (. (setq canvas (canvas-load file +load_flag_film)) :pref_size))
	(def *image_scroll* :min_width w :min_height h)
	(def *window_title* :text (cat "Films -> " (slice file (inc (find-rev "/" file)) -1)))
	(. *image_scroll* :add_child canvas)
	(. *window_title* :layout)
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(def *image_scroll* :min_width 32 :min_height 32)
	(. *window* :change_dirty x y w h))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *main_toolbar*
		'("prev" "next")))

(defun main ()
	(defq select (alloc-select +select_size))
	(tooltips)
	(bind '(x y w h) (apply view-locate (. (win-refresh index) :get_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while id
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event)
				(mail-timeout (elem-get select +select_timer) +rate 0)
				(.-> canvas :next_frame (:swap 0)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((<= +event_prev id +event_next)
				(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length films)) (length films))))
			(:t (. *window* :event *msg*))))
	;close window
	(free-select select)
	(gui-sub-rpc *window*))
