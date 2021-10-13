(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close max min)
	(enum button))

(ui-window *window* ()
	(ui-title-bar _ "Calculator" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-label display (:text "0" :color +argb_white :flow_flags +flow_flag_align_hright :font (create-font "fonts/OpenSans-Regular.ctf" 24)))
	(ui-grid _ (:grid_width 4 :grid_height 4 :color *env_toolbar_col* :font (create-font "fonts/OpenSans-Regular.ctf" 42))
		(each (lambda (text)
			(. (ui-button _ (:text (if (eql text "C") "AC" text))) :connect +event_button))
			"789/456*123-0=C+")))

(defun do_lastop ()
	(cond
		((eql lastop "+")
			(setq accum (+ accum num)))
		((eql lastop "-")
			(setq accum (- accum num)))
		((eql lastop "*")
			(setq accum (* accum num)))
		((eql lastop "/")
			(if (/= num 0) (setq accum (/ accum num)))))
	accum)

(defun main ()
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(defq accum 0 value 0 num 0 lastop nil)
	(while (cond
		((>= (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_button)
			(defq op (get :text (. *window* :find_id (getf msg +ev_msg_action_source_id))))
			(cond
				((eql op "AC")
					(setq accum 0 value 0 num 0 lastop nil))
				((find op "=+-/*")
					(if lastop
						(setq value (do_lastop))
						(setq value num accum num))
					(setq lastop op num 0))
				(t
					(cond
						((= num 0)
							(unless (eql op "0"))
								(setq num (str-as-num op)))
						(t (setq num (str-as-num (cat (str num) op)))))
					(setq value num)))
			(set display :text (str value))
			(.-> display :layout :dirty))
		((= id +event_close)
			;close button
			nil)
		((= id +event_min)
			;min button
			(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
			(. *window* :change_dirty x y w h))
		((= id +event_max)
			;max button
			(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) '(512 512))))
			(. *window* :change_dirty x y w h))
		(t (. *window* :event msg))))
	(gui-sub *window*))
