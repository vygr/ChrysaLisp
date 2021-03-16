;creates a transient request or notification.
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(structure +event 0
	(byte click))

(ui-window window (:border 1)
	(ui-flow btn_menu (:flow_flags +flow_down_fill)
		(ui-grid btn_menu_grid (:grid_width 1 :grid_height 1))))

(defun build-menu (x y pflag)
	(. btn_menu_grid :sub)
	(defq btn_length (length btn_list))
	(def (setq btn_menu_grid (Grid)) :min_width 80 :flow_flags +flow_flag_align_vcenter
		:grid_height btn_length :grid_width 1)
	(each (lambda (c) 
		(def (defq btn (Button)) :text (elem _ btn_list) :min_width 80 :min_height 24)
		(. btn_menu_grid :add_child (. btn :connect +event_click))) (range 0 btn_length))
	(. btn_menu :add_child btn_menu_grid)
	(. btn_menu :layout)
	(bind '(w h) (. btn_menu :pref_size))
	;allows menu to appear downward and upwards in either direction.
	(cond
		((eql pflag :top_left)
			(. window :change x y w h))
		((eql pflag :top_right)
			(. window :change (- x w) y w h))
		((eql pflag :bottom_left)
			(. window :change x (+ y h 2) w h))
		((eql pflag :bottom_right)
			(. window :change (+ (- x w) 2) (+ y h 2) w h))
		(t 	(. window :change x y w h)))
	(. window :dirty_all))

(defun main ()
	;read paramaters from parent
	(bind '(reply_mbox btn_list pos pflag) (mail-read (task-mailbox)))
	(bind '(px py) pos)
	(bind '(x y w h) (apply view-locate (. window :pref_size)))
	(gui-add (. window :change x y w h))
	(build-menu px py pflag)
	(while (cond
		((eql (defq msg (mail-read (task-mailbox))) "") nil)
		((= +event_click (defq id (getf msg +ev_msg_target_id)))
			(defq reply (get :text (. window :find_id (getf msg +ev_msg_action_source_id))))
			(mail-send reply_mbox reply))
		(t (. window :event msg))))
		(. window :hide))