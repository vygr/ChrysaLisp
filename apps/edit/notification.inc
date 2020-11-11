;creates a transient request or notification.
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'click+))

(ui-window window (:border 1)
	(ui-flow _ (:flow_flags +flow_right_fill+)
		(ui-label mylabel (:text "message text"))
		(ui-flow myflow (:flow_flags (logior +flow_flag_align_hright+ +flow_flag_align_vcenter+))
			(ui-grid mygrid (:grid_width 1 :grid_height 1)))))

(defun modal (lbl btns color mx my mw mh)
	(view-sub mygrid)
	(def (setq mygrid (Grid)) :grid_width (length btns) :grid_height 1)
	(each (lambda (x)
		(def (defq b (Button)) :flow_flags +flow_flag_align_hcenter+ :border 1 :text x :border 1)
		(view-add-child mygrid (component-connect b +event_click+))) btns)
	(bind '(w h) (view-pref-size mygrid))
	(view-change mygrid 0 0 w h)
	(view-add-child myflow mygrid)
	; (view-layout myflow)
	; (view-change myflow 0 0 w h)
	(set mylabel :text lbl :color color)
	(view-change-dirty window mx my mw mh))

(defun main ()
	;read paramaters from parent
	(bind '(reply_mbox label_text button_text color sleep_time mx my mw mh) (mail-read (task-mailbox)))
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(defq b_text (split button_text ","))
	(gui-add (view-change window x y w h))
	(modal label_text b_text color mx my mw mh)
	(when sleep_time (task-sleep sleep_time))
	(while (cond
		((eql (defq msg (mail-read (task-mailbox))) "") nil)
		((= (length button_text) 0) (task-sleep sleep_time))
		((= (defq id (get-long msg ev_msg_target_id)) +event_click+)
			(defq reply (get :text (view-find-id window (get-long msg ev_msg_action_source_id))))
			(mail-send reply reply_mbox))
		(t (. window :event msg))))
		(view-hide window))
