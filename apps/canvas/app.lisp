;import settings
(include 'sys/lisp.inc)
(include 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 600 canvas_height 600 canvas_scale 3 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(window-set-title window "Canvas")
(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size window))
(gui-add (view-change window 512 256 w h))

;create child and send args
(mail-send (list canvas (mul canvas_width 1.0) (mul canvas_height 1.0) (mul canvas_scale 1.0))
	(open-child "apps/canvas/child.lisp" kn_call_open))

(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		(t (view-event window msg))))
