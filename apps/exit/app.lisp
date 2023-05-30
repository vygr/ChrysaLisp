(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close confirm))

(ui-window *window* ()
	(ui-title-bar _ "Exit" () ())
	(ui-label _ (:text "Do you wish to exit !" :color +argb_white))
	(ui-grid _ (:grid_height 1)
		(ui-buttons ("Cancel" "Confirm") +event_close)))

(defun position-window ()
	(bind '(w h) (. *window* :pref_size))
	(bind '(pw ph) (. (penv *window*) :get_size))
	(. *window* :change_dirty (/ (- pw w) 2) (/ (- ph h) 2) w h))

(defun main ()
	;add centered
	(gui-add-front *window*)
	(position-window)
	(while (cond
		((and (< (defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) 0)
			(= (getf msg +ev_msg_type) +ev_type_gui))
			;resized GUI
			(position-window))
		((= id +event_close)
			;app close
			:nil)
		((= id +event_confirm)
			;confirm button
			(gui-exit))
		(:t (. *window* :event msg))))
	(gui-sub *window*))
