(import "././login/env.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close logout quit))

(ui-window *window* ()
	(ui-title-bar _ "Logout Manager" () ())
	(ui-label _ (:text (num-to-utf8 0xea47) :color +argb_white
		:font *env_warning_font*
		:flow_flags (logior +flow_flag_align_vcenter +flow_flag_align_hcenter)))
	(ui-label _ (:text "Do you wish to exit?" :color +argb_white))
	(ui-grid _ (:grid_height 1)
		(ui-buttons ("Cancel" "Logout" "Quit") +event_close)))

(defun position-window ()
	(bind '(w h) (. *window* :pref_size))
	(bind '(pw ph) (. (penv *window*) :get_size))
	(. *window* :change_dirty (/ (- pw w) 2) (/ (- ph h) 2) w h))

(defun main ()
	;add centered
	(gui-add-front-rpc *window*)
	(position-window)
	(while (cond
		((and (< (defq id (getf (defq msg (mail-read (task-netid))) +ev_msg_target_id)) 0)
			(= (getf msg +ev_msg_type) +ev_type_gui))
			;resized GUI
			(position-window))
		((= id +event_close)
			;app close
			:nil)
		((= id +event_logout)
			;logout button
			(gui-logout-rpc))
		((= id +event_quit)
			;quit button
			(gui-quit-rpc))
		(:t (. *window* :event msg))))
	(gui-sub-rpc *window*))
