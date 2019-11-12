;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close))

(defq canvas_width 600 canvas_height 600 canvas_scale 1 id t)

(ui-tree window (create-window window_flag_close) nil
	(ui-element canvas (create-canvas canvas_width canvas_height canvas_scale)))

(canvas-set-flags (canvas-fill canvas 0) 1)
(gui-add (apply view-change (cat (list window 512 256)
	(view-pref-size (window-set-title (window-connect-close window event_win_close) "Canvas")))))

;create child and send args
(mail-send (list canvas (* canvas_width 1.0) (* canvas_height 1.0) (* canvas_scale 1.0))
	(defq mbox (open-child "apps/canvas/child.lisp" kn_call_open)))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		(t (view-event window msg))))

;close child and window
(mail-send "" mbox)
(view-hide window)
