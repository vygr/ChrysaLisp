;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

;add event id etc
(defq clock_size 256 clock_scale 1)

;define events we will use
(structure 'event 0
	(byte 'close))

;create a window
(ui-window window ()
	(ui-title-bar _ "Clock" (0xea19) (const event_close))
	(ui-canvas clock clock_size clock_size clock_scale)
	(ui-label display (:text "00:00:00" :color argb_black :ink_color argb_red
		:flow_flags (logior flow_flag_align_hcenter flow_flag_align_vcenter)
		:font (create-font "fonts/Hack-Regular.ctf" 48))))

(defun-bind main ()
	;clear clock face
	(canvas-set-flags (canvas-fill clock 0) 1)
	(gui-add (apply view-change (cat (list window 0 0) (view-pref-size window))))
	;create child and send args
	(mail-send (list display clock (i2f clock_size) (i2f clock_scale))
		(defq mbox (open-child "apps/clock/child.lisp" kn_call_open)))
	;main app loop
	(while (cond
		((= (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id) event_close)
			nil)
		(t (view-event window msg))))
	;close child and window
	(mail-send "" mbox)
	(view-hide window))
