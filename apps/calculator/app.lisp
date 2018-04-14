;import settings
(run 'apps/sys.inc)
(run 'apps/ui.inc)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth))
		(ui-element display (create-label) ('text "0" 'color 0xffffffff 'flow_flags flow_flag_align_hright 'font (create-font "fonts/OpenSans-Regular.ttf" 24)))
		(ui-element _ (create-grid) ('grid_width 4 'grid_height 4 'color 0xffffff00 'font (create-font "fonts/OpenSans-Regular.ttf" 42))
			(each (lambda (text)
				(slot connect_click
					(ui-element _ (create-button) ('text (if (eql text "C") "AC" text) 'flow_flags (bit-or flow_flag_align_hcenter flow_flag_align_vcenter)))
					1)) "789/456*123-0=C+"))))

(slot set_title window "Calculator")
(slot connect_close window 0)
(bind '(w h) (slot pref_size window))
(slot change window 920 48 w h)
(slot gui_add window)

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
		((ge (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) 1)
			(defq op (get (slot find_id window (read-long ev_msg_action_source_id msg)) 'text))
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
			(slot layout display)
			(slot dirty display))
		((eq id 0)
			(setq id nil))
		(t (slot event window msg))))
