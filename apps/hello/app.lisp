;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;define events we will use
(structure 'event 0
	(byte 'close)
	(byte 'click))

;add event id
(defq id t toggle t)

;create a window with a label
(ui-tree window (create-window window_flag_close) nil
	(ui-element button (create-button) ('text "Press Me" 'ink_color argb_blue 'color argb_green
		'flow_flags flow_flag_fillw	'font (create-font "fonts/OpenSans-Regular.ttf" 18)))
	(ui-element label (create-label) ('text "ChrysaLisp" 'color argb_white 'flow_flags flow_flag_fillw
		'font (create-font "fonts/OpenSans-Regular.ttf" 18))))

;set a name to the window
(window-set-title window "Hello")

;bind events
(window-connect-close window event_close)
(button-connect-click button event_click)

;window with at least 200 wide
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 256 128 (add w 200) h))

;main app loop
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_close)
			(setq id nil))
		((eq id event_click)
			(set (view-dirty button) 'color (if (setq toggle (not toggle)) argb_green argb_red))
			(set (view-dirty label) 'text (if toggle "The button is green!" "The button is red!"))
			(view-layout label))
		(t (view-event window msg))))
