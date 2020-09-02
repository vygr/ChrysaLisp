;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(defq clock_size 256 clock_scale 1)

;define events we will use
(structure 'event 0
	(byte 'close))

;create a window
(ui-window window ()
	(ui-title-bar _ "Clock" (0xea19) (const event_close))
	(if (eql *env_clock_analog* t)
		(ui-canvas clock clock_size clock_size clock_scale) (defq clock nil))
	(if (eql *env_clock_digital* t)
		(ui-label display (:text "hh:mm:ss" :color argb_black :ink_color argb_red
		:flow_flags (logior flow_flag_align_hcenter flow_flag_align_vcenter)
		:font (create-font "fonts/Hack-Regular.ctf" 48)))
		(defq display nil))

(defun-bind main ()
	;if analog clear clock face
	(if clock
		(canvas-set-flags (canvas-fill clock 0) 1))
	;create child and send args
	(defq mbox (open-child "apps/clock/child.lisp" kn_call_open))
	;multiply size and scale, as they are only used together in child.
	(mail-send (list display clock (* (i2f clock_size) (i2f clock_scale)))
		mbox)
	(gui-add (apply view-change (cat (list window 0 0) (view-pref-size window))))
	(view-layout window)
	;main app loop
	(while (cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_close)
			nil)
		(t (view-event window msg))))
	;close child and window
	(mail-send "" mbox)
	(view-hide window))
