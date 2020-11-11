;creates a transient request or notification.
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure '+event 0
	(byte 'click+))

(ui-window window (:border 1)
	(ui-flow btn_menu (:flow_flags +flow_down_fill+)
		(ui-grid btn_menu_grid (:grid_width 1 :grid_height 1))))

(defun build-menu (x y pflag)
	(view-sub btn_menu_grid)
	(defq btn_length (length btn_list))
	(def (setq btn_menu_grid (Grid)) :min_width 80 :flow_flags +flow_flag_align_hcenter+ 
		:grid_height btn_length :grid_width 1)
	(each (lambda (c) 
		(def (defq btn (Button)) :text (elem _ btn_list))
		(view-add-child btn_menu_grid (component-connect btn +event_click+))) (range 0 btn_length))
	(view-add-child btn_menu btn_menu_grid)
	(view-layout btn_menu)
	(bind '(w h) (view-pref-size btn_menu))
	;allows menu to appear downward and upwards in either direction.
	(cond
		((eql pflag :top_left)
			(view-change window x y w h))
		((eql pflag :top_right)
			(view-change window (- x w) y w h))
		((eql pflag :bottom_left)
			(view-change window x (+ y h 2) w h))
		((eql pflag :bottom_right)
			(view-change window (+ (- x w) 2) (+ y h 2) w h))
		(t 	(view-change window x y w h)))
	(view-dirty-all window))

(defun main ()
	;read paramaters from parent
	(bind '(reply_mbox btn_list pos pflag) (mail-read (task-mailbox)))
	(bind '(px py) pos)
	(bind '(x y w h) (apply view-locate (view-pref-size window)))
	(gui-add (view-change window x y w h))
	(build-menu px py pflag)
	(while (cond
		((eql (defq msg (mail-read (task-mailbox))) "") nil)
		((= +event_click+ (defq id (get-long msg ev_msg_target_id)))
			(defq reply (get :text (view-find-id window (get-long msg ev_msg_action_source_id))))
			(mail-send reply reply_mbox))
		(t (. window :event msg))))
		(view-hide window))