;import settings
(run 'apps/sys.lisp)
(run 'apps/ui.lisp)

(defq
	canvas_width 600 canvas_height 600 canvas_scale 3.0
	window (create-window window_flag_close)
	canvas (create-canvas canvas_width canvas_height canvas_scale))

(slot set_title window "Canvas")
(slot add_child window canvas)
(slot connect_close window 0)
(bind '(w h) (slot pref_size window))
(slot change window 512 256 w h)
(slot gui_add window)

;create child and send args
(bind '(mbox cpu) (open-child "apps/canvas/child.lisp" kn_call_open))
(mail-send (list canvas (mul canvas_width 1.0) (mul canvas_height 1.0) canvas_scale) mbox cpu)

(defq id t)
(while id
	(cond
		((eq (setq id (read-long ev_msg_target_id (defq msg (mail-mymail)))) 0)
			(setq id nil))
		(t (slot event window msg))))
