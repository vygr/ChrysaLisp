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
	(enum main timer))

;profiling callbacks on the GUI thread from :draw method !!!
(defq *profile_meta_map* (env -1) *profile_return_vals* (list)
	*mouse_x* 0 *mouse_y* 0 *mouse_buttons* 0 *mouse_id* 0
	select (alloc-select +select_size)
	rate (/ 1000000 60) *running* t)

(defun main ()
	;declare service
	(defq service (mail-declare (task-mailbox) "GUI_SERVICE" "GUI Service 0.2"))
	;init screen widget
	(def (defq *screen* (Backdrop)) :style :grid :color +argb_grey2 :ink_color +argb_grey1)
	(.-> *screen* (:change 0 0 1280 960) :dirty_all)
	(gui-init *screen*)
	;fire up the login app and clipboard service
	(open-child "apps/login/app.lisp" +kn_call_open)
	(open-child "apps/clipboard/app.lisp" +kn_call_open)
	(mail-timeout (elem +select_timer select) rate 0)
	(while *running*
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
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
						(. *screen* :add_front view)
						(. view :set_flags +view_flag_dirty_all +view_flag_dirty_all))
					((= cmd 2)
						;add view at back
						(setf view +view_owner_id owner 0)
						(. *screen* :add_back view)
						(. view :set_flags +view_flag_dirty_all +view_flag_dirty_all)))
				(mail-send reply msg)
				(undef (env) 'msg 'view 'owner 'reply))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem +select_timer select) rate 0)
				(gui-update *mouse_x* *mouse_y* 0)
				(while (defq msg (gui-event))
					(if (defq action (. event_map :find (getf msg +sdl_common_event_type)))
						(action)))
				;remove orphans
				(each (# (unless (and (defq owner (. %0 :find_owner)) (mail-validate owner))
						(.-> %0 :hide :sub))) (. *screen* :children)))))
	(free-select select)
	(gui-deinit)
	(mail-forget service))
