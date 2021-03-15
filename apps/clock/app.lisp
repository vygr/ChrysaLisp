;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(defq clock_size 256 clock_scale 1)

;define events we will use
(structure event 0
	(byte close))

;create a window
(ui-window mywindow ()
	(ui-title-bar _ "Clock" (0xea19) +event_close+)
	(if (eql *env_clock_analog* t)
		(ui-canvas clock clock_size clock_size clock_scale) (defq clock nil))
	(if (eql *env_clock_digital* t)
		(ui-label display (:text "xxx hh:mm:ss"
			:flow_flags (logior +flow_flag_align_hcenter+ +flow_flag_align_vcenter+)
			:font (create-font "fonts/Hack-Regular.ctf" 44)))
		(defq display nil)))

(defun main ()
	;if analog clear clock face
	(if clock (.-> clock (:fill 0) (:set_canvas_flags 1)))
	;create child and send args
	(defq mbox (open-child "apps/clock/child.lisp" kn_call_open))
	;multiply size and scale, as they are only used together in child.
	(mail-send mbox (list display clock (* (i2f clock_size) (i2f clock_scale))))
	(bind '(w h) (. mywindow :pref_size))
	(gui-add (. mywindow :change 0 0 w h))
	;main app loop
	(while (cond
		((= (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id+) +event_close+)
			nil)
		(t (. mywindow :event msg))))
	;close child and window
	(mail-send mbox "")
	(. mywindow :hide))
