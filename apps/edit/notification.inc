;creates a transient request or notification.
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/options/options.inc")

(structure '+event 0
	(byte 'click+))

(ui-window notify_window (:border 1)
	(ui-flow _ (:flow_flags flow_right_fill)
		(ui-label label (:text "message text"))
		(ui-flow notify_flow (:flow_flags (logior flow_flag_align_hright flow_flag_align_vcenter))
			(ui-grid grid (:grid_width 1 :grid_height 1)))))

(defun modal ()
	(. grid :sub)
	(def (setq grid (Grid)) :grid_width (length btns) :grid_height 1)
	(each (lambda (x)
		(def (defq b (Button)) :flow_flags flow_flag_align_hcenter :border 1 :text x :border 1)
		(. grid :add_child (component-connect b +event_click+))) btns)
	(bind '(w h) (. grid :pref_size))
	(view-change grid 0 0 w h)
	(. notify_flow :add_child grid)
	;(view-layout flow)
	;(view-change flow 0 0 w h)
	(set label :text lbl :color color)
	(. notify_window :dirty)
	(view-change notify_window mx my mw mh))

(defun main ()
	;read paramaters from parent
	(bind '(reply_mbox lbl btns color sleep_time mx my mw mh) (mail-read (task-mailbox)))
	(options-print "opened")
	(defq b_text (split button_text ","))

	(bind '(x y w h) (apply view-locate (. notify_window :pref_size)))
	(gui-add (view-change notify_window x y w h))
	(modal)
	(when sleep_time (task-sleep sleep_time))
	(while (cond
		((eql (defq msg (mail-read (task-mailbox))) "") nil)
		((= (length button_text) 0) (task-sleep sleep_time))
		((= (defq id (get-long msg ev_msg_target_id)) +event_click+)
			(defq reply (get :text (. notify_window :find_id (get-long msg ev_msg_action_source_id))))
			(mail-send reply reply_mbox))
		(t (. notify_window :event msg))))
	(. notify_window :hide))
