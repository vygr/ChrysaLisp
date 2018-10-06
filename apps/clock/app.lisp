;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;add event id
(defq id t)

;define events we will use
(structure 'event 0
	(byte 'close))

;create a window with a label
(ui-tree window (create-window window_flag_close) nil
	(ui-element display (create-label) ('text "00:00:00" 'color argb_black 'text_color argb_red
		'flow_flags (add flow_flag_align_hcenter flow_flag_align_vcenter)
		'font (create-font "fonts/Hack-Regular.ttf" 48))))

;set a name to the window
(window-set-title window "Clock")

;bind events
(window-connect-close window event_close)

;window width
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 290 16 w h))

;create child and send args
(mail-send (list display) (defq child_id (open-child "apps/clock/child.lisp" kn_call_open)))

;main app loop
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_close)
			(mail-send "" child_id)
			(setq id nil))
		(t (view-event window msg))))
