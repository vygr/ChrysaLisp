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
		((set-mouse-id))))

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

(defun action-mouse-wheel ()
	(mouse-type (defq view (set-mouse-id))
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0)))
	(if (defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_wheel_size)
			(+ev_msg_type +ev_type_wheel)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_wheel_x (getf msg +gui_event_x))
			(+ev_msg_wheel_y (getf msg +gui_event_y))
			(+ev_msg_wheel_direction (getf msg +gui_event_direction))))))

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

(defun action-resized ()
	;the host window is a new size
	(.-> *screen*
		(:set_bounds 0 0 (getf msg +gui_event_x) (getf msg +gui_event_y))
		(:set_flags +view_flag_dirty_all +view_flag_dirty_all))
	(each (lambda (child)
		(mail-send (. child :find_owner)
			(setf-> (str-alloc +ev_msg_gui_size)
				(+ev_msg_type +ev_type_gui)
				(+ev_msg_target_id (. child :get_id))))) (. *screen* :children))
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
	+gui_ev_key_down action-key-down
	+gui_ev_key_up action-key-up
	+gui_ev_resized action-resized
	+gui_ev_shown action-shown
	+gui_ev_quit action-quit))

;module
(export-symbols
	'(*event_map*))
(env-pop)
