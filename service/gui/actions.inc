;module
(env-push)

(import "lib/consts/scodes.inc")

(defq +mod_scodes `'(
		,+sc_lshift ,+sc_rshift ,+sc_lctrl ,+sc_rctrl
		,+sc_lalt ,+sc_ralt ,+sc_lgui ,+sc_rgui
		,+sc_capslock ,+sc_numlockclear)
	+mod_masks `'(
		,+ev_key_mod_left_shift ,+ev_key_mod_right_shift
		,+ev_key_mod_left_control ,+ev_key_mod_right_control
		,+ev_key_mod_left_alt ,+ev_key_mod_right_alt
		,+ev_key_mod_left_meta ,+ev_key_mod_right_meta
		,+ev_key_mod_caps_lock ,+ev_key_mod_num_lock)
	+mod_toggles `'(
		,+sc_capslock ,+sc_numlockclear))

(defun screen-children ()
	; (screen-children) -> (view ...)
	;the views of the screen, the windows, the mouse, the wallpaper. Not one
	;that stands where a view was hidden, (. view :hide), in gui/view: a
	;plain View, hidden, there till the frame is drawn so what was under the
	;view is drawn again, and freed with the frame. It is no window and has
	;no owner of its own, to be closed, told of a theme or a new size
	(filter (# (not (bits? (. %0 :get_flags) +view_flag_hidden))) (. *screen* :children)))

(defun update-mods-down (scode)
	(and (not (find scode +mod_toggles))
		(defq i (find scode +mod_scodes))
		(setq *mods* (logior *mods* (elem-get +mod_masks i)))))

(defun update-mods-up (scode)
	(if (defq i (find scode +mod_scodes))
		(if (find scode +mod_toggles)
			(setq *mods* (logxor *mods* (elem-get +mod_masks i)))
			(setq *mods* (logand *mods* (lognot (elem-get +mod_masks i)))))))

(defun cook-key (scode)
	(defq key :nil)
	(if (/= (logand *mods* +ev_key_mod_shift) 0)
		(setq key (. *shift_map* :find scode)))
	(unless key (setq key (. *normal_map* :find scode)))
	(unless key (setq key 0))
	(if (/= (logand *mods* +ev_key_mod_caps_lock) 0)
		(setq key (ascii-upper key)))
	key)

(defun set-mouse-id ()
	(bind '(view & &) (. *screen* :hit_tree *mouse_x* *mouse_y*))
	(unless view (setq view *screen*))
	(when (/= (defq mouse_id (. view :get_id)) *mouse_id*)
		(and (defq old_view (. *screen* :find_id *mouse_id*))
			(defq owner (. old_view :find_owner))
			(mail-send owner (setf-> (str-alloc +ev_msg_exit_size)
				(+ev_msg_type +ev_type_exit)
				(+ev_msg_target_id *mouse_id*))))
		(setq *mouse_id* mouse_id)
		(and (defq owner (. view :find_owner))
			(mail-send owner (setf-> (str-alloc +ev_msg_enter_size)
				(+ev_msg_type +ev_type_enter)
				(+ev_msg_target_id *mouse_id*))))) view)

(defun held-mouse-id ()
	;the view the mouse is on. While a button is held that is the view it
	;went down on, a window being dragged can lag behind the pointer, and
	;to look under the pointer again would let go of it
	(or (and (/= *mouse_buttons* 0) (. *screen* :find_id *mouse_id*))
		(set-mouse-id)))

(defun set-focus-id ()
	(cond
		(*focus*
			(and (defq old_view (. *screen* :find_id *mouse_id*))
				(nql old_view *focus*)
				(defq owner (. old_view :find_owner))
				(mail-send owner (setf-> (str-alloc +ev_msg_exit_size)
					(+ev_msg_type +ev_type_exit)
					(+ev_msg_target_id *mouse_id*))))
			(setq *mouse_id* (. *focus* :get_id))
			*focus*)
		((held-mouse-id))))

(defun action-key-down ()
	(defq view (set-focus-id)
		scode (getf msg +gui_event_scode))
	(update-mods-down scode)
	;(prin scode)(print)
	(defq owner (. view :find_owner))
	(when owner
		(. *key_dispatch* :insert scode (list *mouse_id* owner)))
	(mouse-type view
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0)))
	(when owner
		(mail-send owner (setf-> (str-alloc +ev_msg_key_size)
			(+ev_msg_type +ev_type_key_down)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_key_scode scode)
			(+ev_msg_key_key (cook-key scode))
			(+ev_msg_key_mod *mods*)))))

(defun action-key-up ()
	(defq view (set-focus-id)
		scode (getf msg +gui_event_scode))
	(update-mods-up scode)
	(when (defq entry (. *key_dispatch* :find scode))
		(. *key_dispatch* :erase scode)
		(bind '(old_id owner) entry)
		(mail-send owner (setf-> (str-alloc +ev_msg_key_size)
			(+ev_msg_type +ev_type_key_up)
			(+ev_msg_target_id old_id)
			(+ev_msg_key_scode scode)
			(+ev_msg_key_key (cook-key scode))
			(+ev_msg_key_mod *mods*)))))

;A turn of the wheel is for the view under the mouse as it turns, or the one
;a button went down on while held. Chris: "you should never be able to have
;a scroll region consume wheel events that were over another scroll region,
;regardless of the speed of the scrolling !" But the host goes on sending
;turns after the hand has stopped, a pad's fling runs on, or they were
;queued, and a view the mouse moved over was scrolled with nobody turning
;anything. Chris: "drop them". While turns come less than +wheel_gap apart,
;microseconds, those over another view than the one that takes them, the
;nearest up from the mouse with a :mouse_wheel, a Scroll, go to nobody. The
;first after a pause goes to the one under the mouse, and so does one that
;is a new push, not what runs on: the other way, or bigger by +wheel_push
;than the turn before. What runs on only dies away, by a step of 1 or so;
;a Magic Mouse's own turns over the other view, logged, came the other way
;or jumped from 1 to 4 and up. Queued turns are the same size, and dropped
(defq +wheel_gap 350000 +wheel_push 3 *wheel_id* :nil *wheel_time* 0 *wheel_turn* '(0 0))

(defun wheel-push? (dx dy)
	; (wheel-push? dx dy) -> :t | :nil
	;a turn not running on from the one before: the other way, or bigger
	(bind '(ox oy) *wheel_turn*)
	(or (< (* dx ox) 0) (< (* dy oy) 0)
		(>= (+ (abs dx) (abs dy)) (+ (abs ox) (abs oy) +wheel_push))))

(defun wheel-view (view)
	; (wheel-view view) -> view
	;what takes a turn over view, the nearest up with a :mouse_wheel, else it
	(defq at view)
	(while (and at (not (.? at :mouse_wheel))) (setq at (penv at)))
	(ifn at view))

(defun action-mouse-wheel ()
	(mouse-type (defq view (held-mouse-id))
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0)))
	(defq id *mouse_id* now (pii-time) to (. (wheel-view view) :get_id)
		dx (getf msg +gui_event_x) dy (getf msg +gui_event_y)
		running (and (< (- now *wheel_time*) +wheel_gap) (not (wheel-push? dx dy))))
	(setq *wheel_time* now *wheel_turn* (list dx dy))
	(unless (and running *wheel_id* (/= to *wheel_id*))
		(setq *wheel_id* to)
		(if (defq owner (. view :find_owner))
			(mail-send owner (setf-> (str-alloc +ev_msg_wheel_size)
				(+ev_msg_type +ev_type_wheel)
				(+ev_msg_target_id id)
				;the way round the user has it, usr/Guest/env.inc
				(+ev_msg_wheel_x (* (get :wheel_x *env*) (getf msg +gui_event_x)))
				(+ev_msg_wheel_y (* (get :wheel_y *env*) (getf msg +gui_event_y)))
				(+ev_msg_wheel_direction (getf msg +gui_event_direction)))))))

(defun action-mouse-motion ()
	(setq *mouse_x* (getf msg +gui_event_x)
		*mouse_y* (getf msg +gui_event_y)
		*mouse_buttons* (getf msg +gui_event_buttons))
	(when (= *mouse_buttons* 0) (set-mouse-id))
	(and (defq view (. *screen* :find_id *mouse_id*))
		(defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_mouse_size)
			(+ev_msg_type +ev_type_mouse)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_mouse_x *mouse_x*)
			(+ev_msg_mouse_y *mouse_y*)
			(+ev_msg_mouse_rx (- *mouse_x* (getf view +view_ctx_x 0)))
			(+ev_msg_mouse_ry (- *mouse_y* (getf view +view_ctx_y 0)))
			(+ev_msg_mouse_buttons *mouse_buttons*)
			(+ev_msg_mouse_count 0))))
	(mouse-type view
		(- *mouse_x* (if view (getf view +view_ctx_x 0) 0))
		(- *mouse_y* (if view (getf view +view_ctx_y 0) 0))))

(defun action-mouse-button-down ()
	(setq *mouse_x* (getf msg +gui_event_x)
		*mouse_y* (getf msg +gui_event_y)
		*mouse_buttons* (getf msg +gui_event_buttons))
	(when (and (defq view (set-mouse-id))
			(defq owner (. view :find_owner)))
		(mail-send owner (setf-> (str-alloc +ev_msg_mouse_size)
			(+ev_msg_type +ev_type_mouse)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_mouse_x *mouse_x*)
			(+ev_msg_mouse_y *mouse_y*)
			(+ev_msg_mouse_rx (- *mouse_x* (getf view +view_ctx_x 0)))
			(+ev_msg_mouse_ry (- *mouse_y* (getf view +view_ctx_y 0)))
			(+ev_msg_mouse_buttons *mouse_buttons*)
			(+ev_msg_mouse_count (getf msg +gui_event_count))))
		(setq *focus* (if (Textfield? view) view))))

(defun action-mouse-button-up ()
	(setq *mouse_x* (getf msg +gui_event_x)
		*mouse_y* (getf msg +gui_event_y)
		*mouse_buttons* (getf msg +gui_event_buttons))
	(and (defq view (. *screen* :find_id *mouse_id*))
		(defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_mouse_size)
			(+ev_msg_type +ev_type_mouse)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_mouse_x *mouse_x*)
			(+ev_msg_mouse_y *mouse_y*)
			(+ev_msg_mouse_rx (- *mouse_x* (getf view +view_ctx_x 0)))
			(+ev_msg_mouse_ry (- *mouse_y* (getf view +view_ctx_y 0)))
			(+ev_msg_mouse_buttons 0)
			(+ev_msg_mouse_count (getf msg +gui_event_count)))))
	(mouse-type (defq view (set-mouse-id))
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0))))

;the view each pen or finger went down on, by its id. It is that view's
;till it comes up, wherever it goes
(defq *pointer_views* (Fmap 11))

(defun action-pointer ()
	;a pen or a finger. It goes to the view it is on, or the one it went
	;down on while it is down. The mouse is not moved by it
	(defq id (getf msg +gui_event_id) type (getf msg +gui_event_type)
		x (getf msg +gui_event_x) y (getf msg +gui_event_y)
		view (if (defq held (. *pointer_views* :find id)) (. *screen* :find_id held)))
	(unless view
		(bind '(view & &) (. *screen* :hit_tree x y))
		(unless view (setq view *screen*)))
	(cond
		((= type +gui_ev_pointer_down)
			(. *pointer_views* :insert id (. view :get_id))
			;what is typed goes to a text field that is touched
			(setq *focus* (if (Textfield? view) view)))
		((= type +gui_ev_pointer_up)
			(. *pointer_views* :erase id)))
	(when (defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_pointer_size)
			(+ev_msg_type +ev_type_pointer)
			(+ev_msg_target_id (. view :get_id))
			(+ev_msg_pointer_id id)
			(+ev_msg_pointer_kind (getf msg +gui_event_kind))
			(+ev_msg_pointer_buttons (getf msg +gui_event_buttons))
			(+ev_msg_pointer_pressure (getf msg +gui_event_pressure))
			(+ev_msg_pointer_x x)
			(+ev_msg_pointer_y y)
			(+ev_msg_pointer_rx (- x (getf view +view_ctx_x 0)))
			(+ev_msg_pointer_ry (- y (getf view +view_ctx_y 0)))))))

(defun action-resized ()
	;the host window is a new size
	(.-> *screen*
		(:set_bounds 0 0 (getf msg +gui_event_x) (getf msg +gui_event_y))
		(:set_flags +view_flag_dirty_all +view_flag_dirty_all))
	(each (lambda (child)
		(mail-send (. child :find_owner)
			(setf-> (str-alloc +ev_msg_action_size)
				(+ev_msg_type +ev_type_action)
				(+ev_msg_target_id +event_layout)
				(+ev_msg_action_source_id (. child :get_id))))) (screen-children))
	(gui-update *mouse_x* *mouse_y* 1))

(defun action-shown ()
	;the host window is on show again
	(. *screen* :set_flags +view_flag_dirty_all +view_flag_dirty_all))

(defun action-quit ()
	;launch logout app
	(open-child "apps/system/logout/app.lisp" +kn_call_pin))

(defq *event_map* (scatter (Fmap)
	+gui_ev_mouse_wheel action-mouse-wheel
	+gui_ev_mouse_motion action-mouse-motion
	+gui_ev_mouse_down action-mouse-button-down
	+gui_ev_mouse_up action-mouse-button-up
	+gui_ev_pointer_down action-pointer
	+gui_ev_pointer_motion action-pointer
	+gui_ev_pointer_up action-pointer
	+gui_ev_key_down action-key-down
	+gui_ev_key_up action-key-up
	+gui_ev_resized action-resized
	+gui_ev_shown action-shown
	+gui_ev_quit action-quit))

;module
;*pointer_views* with it: what an action is, is done later, by the GUI's
;loop, where what is named in it is looked for then. A function was found
;as this was read. A thing kept here was not, and the first pen or finger
;there ever was on a desktop stopped the GUI, it was not bound
(export-symbols
	'(*event_map* *pointer_views* *wheel_id* *wheel_time* *wheel_turn* screen-children))
(env-pop)
