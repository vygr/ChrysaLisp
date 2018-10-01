; the upmost basic requirements to an app.
; import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

; add close event
(structure 'event 0
	(byte 'win_close))

; add window id
(defq id t)

; create a window with a label
(ui-tree window (create-window window_flag_close) nil
	(ui-element display (create-label) ('text "ChrysaLisp" 'color argb_white 'flow_flags flow_flag_align_hleft 'font (create-font "fonts/OpenSans-Regular.ttf" 18))))

; set a name to the window
(window-set-title window "Hello")

; bind close event
(window-connect-close window event_win_close)

; window should be at last 200 wide
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 256 128 (add w 200) h))

; main app loop
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_close)
			(setq id nil))
		(t (view-event window msg))))
