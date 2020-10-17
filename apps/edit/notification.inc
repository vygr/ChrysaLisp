;creates a transient request or notification.
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'click+))

(ui-window window (:border 1)
	(ui-flow _ (:flow_flags flow_right_fill)
		(ui-label label (:text "message text"))
		(ui-flow flow (:flow_flags (logior flow_flag_align_hright flow_flag_align_vcenter))
			(ui-grid grid (:grid_width 1 :grid_height 1)))))

(defun-bind modal (lbl btns color mx my mw mh)
	(view-sub grid)
	(def (setq grid (create-grid)) :grid_width (length btns) :grid_height 1)
	(each (lambda (x)
		(def (defq b (create-button)) :flow_flags flow_flag_align_hcenter :border 1 :text x :border 1)
		(view-add-child grid (component-connect b +event_click+))) btns)
	(bind '(w h) (view-pref-size grid))
	(view-change grid 0 0 w h)
	(view-add-child flow grid)
	;(view-layout flow)
	;(view-change flow 0 0 w h)
	(set label :text lbl :color color)
	(view-change-dirty window mx my mw mh))

(defun-bind main ()
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
		(t (view-event window msg))))
		(view-hide window))