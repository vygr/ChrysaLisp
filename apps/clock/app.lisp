;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;add event id
(defq id t)

(defun make_time ()
	(defq sec (div (time) 1000000) seconds (mod sec 60)
		minutes (mod (div sec 60) 60) hours (mod (div sec 60 60) 24))
	(cat (pad hours 2 "0") ":" (pad minutes 2 "0") ":" (pad seconds 2 "0")))

;define events we will use
(structure 'event 0
	(byte 'close))

;create a window with a label
(ui-tree window (create-window window_flag_close) nil
	(ui-element label (create-label) ('text (make_time) 'color argb_yellow 'flow_flags flow_flag_fillw
		'font (create-font "fonts/Hack-Regular.ttf" 32))))

;set a name to the window
(window-set-title window "Clock")

;bind events
(window-connect-close window event_close)

;window width
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 290 16 w h))

;main app loop
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_close)
			(setq id nil))
		(t (view-event window msg))))
