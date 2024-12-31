(defq _ (env))
;import into the shared boot env of this node !
;comment next two line out if need to profile...
(defq _ *root_env*)
(import "sys/lisp.inc" _)
(import "class/lisp.inc" _)
(import "gui/lisp.inc" _)
(undef (env) '_)
(task-slice)

(import "sys/pii/lisp.inc")
(defq *env_user* "Guest")
(import "apps/login/Guest/env.inc")
(import *env_keyboard_map*)
(import "./actions.inc")

(enums +select 0
	(enum main timer mouse))

;profiling and stack frame callbacks on the GUI thread from :draw method etc !!!
(setq *profile_map* (env 1) *stack_frame* (list) *debug_state* :gui)

;frame rate
(defq +rate (/ 1000000 60))

(defun mouse-type (view rx ry)
	(if view
		(defq mouse_type (case (pop (. view :type_of))
			(:Title 13)
			((:Vdu :Textfield) 2)
			(:Slider 12)
			(:Window
				(case (if (= *mouse_buttons* 0)
						(first (. view :drag_mode rx ry))
						(get :drag_mode view))
					(1 10) (2 8) (3 5) (4 10) (6 4) (8 8) (9 4) (12 5) (:t 0)))
			(:t 0)))
		(defq mouse_type 0))
	(when (or (/= *mouse_x* *old_mouse_x*)
			(/= *mouse_y* *old_mouse_y*)
			(/= *mouse_type* mouse_type))
		(setq *old_mouse_x* *mouse_x* *old_mouse_y* *mouse_y* *mouse_type* mouse_type)
		(bind '(w h) (. *mouse* :pref_size))
		(def *mouse* :offset_x 0 :offset_y (* mouse_type w -1))
		(bind '(hx hy) (case mouse_type (0 '(0 0)) (12 '(6 0)) (:t '(8 8))))
		(. *mouse* :change_dirty (- *mouse_x* hx) (- *mouse_y* hy) w w)))

(defun dispatch (msg type)
	(and (defq action (. *event_map* :find type)) (action)))

(defun close-apps (quit)
	;send quit action to all GUI apps
	;action 0 is reservered for close !
	(each (lambda (child)
		(when (defq mbox (. child :find_owner))
			(defq source_id (. child :get_id))
			(mail-send mbox (setf-> (str-alloc +ev_msg_action_size)
				(+ev_msg_type +ev_type_action)
				(+ev_msg_target_id 0)
				(+ev_msg_action_source_id source_id)))))
		(. *screen* :children))
	;run login app or quit ?
	(if quit
		(setq *quiting* :t)
		(open-child "apps/login/app.lisp" +kn_call_open)))

(defun main ()
	;declare service and vars
	(defq select (alloc-select +select_size)
		service (mail-declare (task-netid) "Gui" "GUI Service 0.2")
		*running* :t *quiting* :nil *old_mouse_x* -1 *old_mouse_y* -1
		*mouse_type* 0 *mouse_x* 0 *mouse_y* 0 *mouse_buttons* 0 *mouse_id* 0
		*mods* 0 *key_dispatch* (Fmap) *focus* :nil)
	;init screen widget
	(def (defq *screen* (Backdrop)) :style :grid :color +argb_grey2 :ink_color +argb_grey1)
	(. (gui-init (. *screen* :change *env_window_x* *env_window_y* *env_window_width* *env_window_height*)) :dirty_all)
	(gui-update 0 0 0)
	;init mouse widget
	(defq *mouse* (canvas-load "apps/images/data/mice.cpm" 0))
	(setf *mouse* +view_owner_id (elem-get select +select_mouse) 0)
	(. *mouse* :set_flags +view_flag_at_front (const (+ +view_flag_solid +view_flag_at_front)))
	(. *screen* :add_front *mouse*)
	(mouse-type *screen* 0 0)
	;fire up the login app and clipboard service
	(open-child "apps/login/app.lisp" +kn_call_open)
	(open-child "service/clipboard/app.lisp" +kn_call_open)
	(open-child "service/audio/app.lisp" +kn_call_open)
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(let* ((idx (mail-select select))
			  (msg (mail-read (elem-get select idx))))
			(cond
				((= idx +select_main)
					;main mailbox
					(bind '(cmd view owner reply) msg)
					(cond
						((= cmd 0)
							;quit all
							(close-apps :t))
						((= cmd 1)
							;quit all, restart login
							(close-apps :nil))
						((= cmd 2)
							;hide and sub view
							(.-> view :hide :sub))
						((= cmd 3)
							;add view at front
							(. view :set_owner owner)
							(. *screen* :add_back view)
							(. view :to_front))
						((= cmd 4)
							;add view at back
							(. view :set_owner owner)
							(. *screen* :add_back view)
							(. view :set_flags +view_flag_dirty_all +view_flag_dirty_all)))
					(mail-send reply msg))
				((= idx +select_mouse)
					;mouse mailbox
					)
				((= idx +select_timer)
					;timer event
					(mail-timeout (elem-get select +select_timer) +rate 0)
					(gui-update *mouse_x* *mouse_y* 0)
					;dispatch events, roll up mouse motion
					(defq last_motion :nil)
					(while (defq msg (gui-event))
						(cond
							((= (defq type (getf msg +sdl_common_event_type)) +SDL_MOUSEMOTION)
								(setq last_motion msg))
							(:t (when last_motion
									(dispatch last_motion +SDL_MOUSEMOTION)
									(setq last_motion :nil))
								(dispatch msg type))))
					(when last_motion
						(dispatch last_motion +SDL_MOUSEMOTION))
					;remove orphans
					(each (# (unless (and (defq owner (. %0 :find_owner)) (mail-validate owner))
							(.-> %0 :hide :sub))) (defq children (. *screen* :children)))
					;quit if no apps
					(and *quiting* (<= (length children) 1) (setq *running* :nil))))))
	(mail-forget service)
	(free-select select)
	(gui-deinit))
