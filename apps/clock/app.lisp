;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

;add event id etc
(defq clock_size 256 clock_scale 2 id t)

;define events we will use
(structure 'event 0
	(byte 'close))

;create a window with a label
(ui-tree window (create-window window_flag_close) nil
	(ui-element display (create-label) ('text "00:00:00" 'color argb_black 'ink_color argb_red
		'flow_flags (add flow_flag_align_hcenter flow_flag_align_vcenter)
		'font (create-font "fonts/Hack-Regular.ttf" 48)))
	(ui-element clock (create-canvas clock_size clock_size clock_scale)))

;set a name to the window and clear clock face
(window-set-title window "Clock")
(canvas-fill clock 0)

;bind events
(window-connect-close window event_close)

;window width
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 290 16 w h))

;create child and send args
(mail-send (list display clock (mul clock_size 1.0) (mul clock_scale 1.0))
	(defq child_id (open-child "apps/clock/child.lisp" kn_call_open)))

;main app loop
(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_close)
			(mail-send "" child_id)
			(setq id nil))
		(t (view-event window msg))))
