;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'close))

(defq canvas_width 600 canvas_height 600 canvas_scale 1)

(ui-window window ()
	(ui-title-bar _ "Canvas" (0xea19) (const event_close))
	(ui-canvas canvas canvas_width canvas_height canvas_scale))

(defun-bind main ()
	(canvas-set-flags (canvas-fill canvas 0) 1)
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(gui-add (view-change window x y w h))
	;create child and send args
	(mail-send (list canvas (i2f canvas_width) (i2f canvas_height) (i2f canvas_scale))
		(defq mbox (open-child "apps/canvas/child.lisp" kn_call_open)))
	(while (cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_close)
			nil)
		(t (view-event window msg))))
	;close child and window
	(mail-send "" mbox)
	(view-hide window))
