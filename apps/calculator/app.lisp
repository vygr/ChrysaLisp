;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_min)
	(byte 'win_max)
	(byte 'win_button))

(ui-tree window (create-window (add window_flag_close window_flag_min window_flag_max)) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth))
		(ui-element display (create-label) ('text "0" 'color 0xffffffff 'flow_flags flow_flag_align_hright 'font (create-font "fonts/OpenSans-Regular.ttf" 24)))
		(ui-element _ (create-grid) ('grid_width 4 'grid_height 4 'color 0xffffff00 'font (create-font "fonts/OpenSans-Regular.ttf" 42))
			(each (lambda (text)
				(button-connect-click
					(ui-element _ (create-button) ('text (if (eql text "C") "AC" text)))
					event_win_button)) "789/456*123-0=C+"))))

(window-set-title window "Calculator")
(window-connect-close window event_win_close)
(window-connect-min window event_win_min)
(window-connect-max window event_win_max)
(bind '(w h) (view-pref-size window))
(view-change window 920 48 w h)
(gui-add window)

(defun do_lastop ()
	(cond
		((eql lastop "+")
			(setq accum (add accum num)))
		((eql lastop "-")
			(setq accum (sub accum num)))
		((eql lastop "*")
			(setq accum (mul accum num)))
		((eql lastop "/")
			(if (ne num 0) (setq accum (div accum num)))))
	accum)

(defq id t accum 0 value 0 num 0 lastop nil)
(while id
	(cond
		((ge (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_button)
			(defq op (get (view-find-id window (read-long ev_msg_action_source_id msg)) 'text))
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
						((eq num 0)
							(unless (eql op "0"))
								(setq num (to-num op)))
						(t (setq num (to-num (cat (str num) op)))))
					(setq value num)))
			(set display 'text (str value))
			(view-layout display)
			(view-dirty display))
		((eq id event_win_close)
			;close button
			(setq id nil))
		((eq id event_win_min)
			;min button
			(view-dirty window)
			(bind '(x y _ _) (view-get-bounds window))
			(bind '(w h) (view-pref-size window))
			(view-change window x y w h)
			(view-dirty-all window))
		((eq id event_win_max)
			;max button
			(view-dirty window)
			(bind '(x y _ _) (view-get-bounds window))
			(view-change window x y 512 512)
			(view-dirty-all window))
		(t (view-event window msg))))
