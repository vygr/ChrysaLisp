;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max 'win_button))

(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max)) nil
	(ui-element display (create-label) ('text "0" 'color argb_white 'flow_flags flow_flag_align_hright
		'font (create-font "fonts/OpenSans-Regular.ctf" 24)))
	(ui-element _ (create-grid) ('grid_width 4 'grid_height 4 'color toolbar_col
			'font (create-font "fonts/OpenSans-Regular.ctf" 42))
		(each (lambda (text)
			(component-connect
				(ui-element _ (create-button) ('text (if (eql text "C") "AC" text)))
				event_win_button)) "789/456*123-0=C+")))

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

(defun-bind main ()
	(gui-add (apply view-change (cat (list window 920 48)
		(view-pref-size (window-set-title (window-connect-close (window-connect-min
			(window-connect-max window event_win_max) event_win_min) event_win_close) "Calculator")))))
	(defq id t accum 0 value 0 num 0 lastop nil)
	(while id
		(cond
			((>= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_button)
				(defq op (get (view-find-id window (get-long msg ev_msg_action_source_id)) 'text))
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
									(setq num (str-to-num op)))
							(t (setq num (str-to-num (cat (str num) op)))))
						(setq value num)))
				(set display 'text (str value))
				(view-dirty (view-layout display)))
			((= id event_win_close)
				;close button
				(setq id nil))
			((= id event_win_min)
				;min button
				(bind '(x y _ _) (view-get-bounds window))
				(bind '(w h) (view-pref-size window))
				(view-change-dirty window x y w h))
			((= id event_win_max)
				;max button
				(bind '(x y _ _) (view-get-bounds window))
				(view-change-dirty window x y 512 512))
			(t (view-event window msg))))
	(view-hide window))
