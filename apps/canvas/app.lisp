;import settings
(run 'apps/sys.inc)
(run 'apps/ui.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 600 canvas_height 600 canvas_scale 3.0 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(slot set_title window "Canvas")
(slot connect_close window event_win_close)
(bind '(w h) (slot pref_size window))
(slot change window 512 256 w h)
(slot gui_add window)

;create child and send args
(mail-send (list canvas (mul canvas_width 1.0) (mul canvas_height 1.0) canvas_scale)
	(open-child "apps/canvas/child.lisp" kn_call_open))

(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) event_win_close)
			(setq id nil))
		(t (slot event window msg))))
