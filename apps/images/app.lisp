(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/files/files.inc")

(enums +event 0
	(enum close)
	(enum prev next))

(enums +select 0
	(enum main tip))

(defq images (sort (files-all "apps/images/data" '(".cpm" ".tga" ".svg")))
	index (some (# (if (eql "apps/images/data/tiger.svg" %0) (!))) images)
	id :t)

(ui-window *window* ()
	(ui-title-bar *window_title* "" (0xea19) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll *image_scroll* +scroll_flag_both))

(defun win-refresh (_)
	(defq file (elem-get images (setq index _)))
	(bind '(w h) (. (defq canvas (canvas-load file 0)) :pref_size))
	(def *image_scroll* :min_width w :min_height h)
	(def *window_title* :text (cat "Images -> " (slice file (inc (find-rev "/" file)) -1)))
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
	(gui-add-front (. *window* :change x y w h))
	(while id
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			((<= +event_prev id +event_next)
				(win-refresh (% (+ index (dec (* 2 (- id +event_prev))) (length images)) (length images))))
			(:t (. *window* :event *msg*))))
	(free-select select)
	(gui-sub *window*))
