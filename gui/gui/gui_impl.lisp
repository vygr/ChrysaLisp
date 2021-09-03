;(import "lib/debug/frames.inc")
;import into the shared root env of this node !
(defq _ (env))
;comment next two lines out if need to...
(while (penv _) (setq _ (penv _)))
(eval '(env 307) _)
(import "sys/lisp.inc" _)
(import "class/lisp.inc" _)
(import "gui/lisp.inc" _)

(import "sys/pii/lisp.inc")
(import "./actions.inc")

(enums +select 0
	(enum main timer mouse))

;profiling callbacks on the GUI thread from :draw method !!!
(defq *profile_meta_map* (env -1) *profile_return_vals* (list)
	*old_mouse_x* -1 *old_mouse_y* -1 *mouse_type* 0
	*mouse_x* 0 *mouse_y* 0 *mouse_buttons* 0 *mouse_id* 0
	rate (/ 1000000 60) *running* t)

(defun mouse-type (view rx ry)
	(if view
		(defq mouse_type (case (pop (. view :type_of))
			(:Title 13)
			((:Vdu :Textfield) 2)
			(:Slider 12)
			(:Window
				(case (if (= *mouse_buttons* 0)
						(elem 0 (. view :drag_mode rx ry))
						(get :drag_mode view))
					(1 10) (2 8) (3 5) (4 10) (6 4) (8 8) (9 4) (12 5) (t 0)))
			(t 0)))
		(defq mouse_type 0))
	(when (or (/= *mouse_x* *old_mouse_x*)
			(/= *mouse_y* *old_mouse_y*)
			(/= *mouse_type* mouse_type))
		(setq *old_mouse_x* *mouse_x* *old_mouse_y* *mouse_y* *mouse_type* mouse_type)
		(bind '(w h) (. *mouse* :pref_size))
		(def *mouse* :offset_x 0 :offset_y (* mouse_type w -1))
		(bind '(hx hy) (case mouse_type (0 '(0 0)) (12 '(6 0)) (t '(8 8))))
		(. *mouse* :change_dirty (- *mouse_x* hx) (- *mouse_y* hy) w w)))

(defun main ()
	;declare service
	(defq select (alloc-select +select_size)
		service (mail-declare (task-mailbox) "GUI_SERVICE" "GUI Service 0.2"))
	;init screen widget
	(def (defq *screen* (Backdrop)) :style :grid :color +argb_grey2 :ink_color +argb_grey1)
	(.-> *screen* (:change 0 0 1280 960) :dirty_all)
	(gui-init *screen*)
	(gui-update 0 0 0)
	;init mouse widget
	(defq *mouse* (Canvas-from-file "apps/images/data/mice.cpm" 0))
	(setf *mouse* +view_owner_id (elem +select_mouse select) 0)
	(. *mouse* :set_flags +view_flag_at_front (const (+ +view_flag_solid +view_flag_at_front)))
	(. *screen* :add_front *mouse*)
	(mouse-type *screen* 0 0)
	;fire up the login app and clipboard service
	(open-child "apps/login/app.lisp" +kn_call_open)
	(open-child "apps/clipboard/app.lisp" +kn_call_open)
	(mail-timeout (elem +select_timer select) rate 0)
	(while *running*
		(let* ((idx (mail-select select))
			  (msg (mail-read (elem idx select))))
			(cond
				((= idx +select_main)
					;main mailbox
					(bind '(cmd view owner reply) msg)
					(cond
						((= cmd 0)
							;hide and sub view
							(.-> view :hide :sub))
						((= cmd 1)
							;add view at front
							(setf view +view_owner_id owner 0)
							(. *screen* :add_back view)
							(. view :to_front))
						((= cmd 2)
							;add view at back
							(setf view +view_owner_id owner 0)
							(. *screen* :add_back view)
							(. view :set_flags +view_flag_dirty_all +view_flag_dirty_all)))
					(mail-send reply msg))
				((= idx +select_mouse)
					;mouse mailbox
					)
				((= idx +select_timer)
					;timer event
					(mail-timeout (elem +select_timer select) rate 0)
					(gui-update *mouse_x* *mouse_y* 0)
					(while (defq msg (gui-event))
						(if (defq action (. event_map :find (getf msg +sdl_common_event_type)))
							(action)))
					;remove orphans
					(each (# (unless (and (defq owner (. %0 :find_owner)) (mail-validate owner))
							(.-> %0 :hide :sub))) (. *screen* :children))))))
	(free-select select)
	(gui-deinit)
	(mail-forget service))
