;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'close+))

(defq canvas_width 600 canvas_height 600 canvas_scale 1)

(ui-window mywindow ()
	(ui-title-bar _ "Canvas" (0xea19) +event_close+)
	(ui-canvas canvas canvas_width canvas_height canvas_scale))

(defun main ()
	(. (. canvas :fill 0) :set_canvas_flags 1)
	(bind '(x y w h) (apply view-locate (. mywindow :pref_size)))
	(gui-add (view-change mywindow x y w h))
	;create child and send args
	(mail-send (list canvas (i2f canvas_width) (i2f canvas_height) (i2f canvas_scale))
		(defq mbox (open-child "apps/canvas/child.lisp" kn_call_open)))
	(while (cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) +event_close+)
			nil)
		(t (. mywindow :event msg))))
	;close child and mywindow
	(mail-send "" mbox)
	(. mywindow :hide))
