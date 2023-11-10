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
		(setq *mods* (logior *mods* (elem-get i +mod_masks)))))

(defun update-mods-up (scode)
	(if (defq i (find scode +mod_scodes))
		(if (find scode +mod_toggles)
			(setq *mods* (logxor *mods* (elem-get i +mod_masks)))
			(setq *mods* (logand *mods* (lognot (elem-get i +mod_masks)))))))

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
	(bind '(view _ _) (. *screen* :hit_tree *mouse_x* *mouse_y*))
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
				(not (eql old_view *focus*))
				(defq owner (. old_view :find_owner))
				(mail-send owner (setf-> (str-alloc +ev_msg_exit_size)
					(+ev_msg_type +ev_type_exit)
					(+ev_msg_target_id *mouse_id*))))
			(setq *mouse_id* (. *focus* :get_id))
			*focus*)
		((set-mouse-id))))

(defun action-key-down ()
	(defq view (set-focus-id)
		scode (getf msg +sdl_keyboard_event_scode))
	(update-mods-down scode)
	;(prin scode)(print)
	(. *key_dispatch* :insert scode *mouse_id*)
	(mouse-type view
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0)))
	(when (defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_key_size)
			(+ev_msg_type +ev_type_key_down)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_key_scode scode)
			(+ev_msg_key_key (cook-key scode))
			(+ev_msg_key_mod *mods*)))))

(defun action-key-up ()
	(defq view (set-focus-id)
		scode (getf msg +sdl_keyboard_event_scode))
	(update-mods-up scode)
	(when (defq old_id (. *key_dispatch* :find scode))
		(. *key_dispatch* :erase scode)
		(and (defq view (. *screen* :find_id old_id))
			(defq owner (. view :find_owner))
			(mail-send owner (setf-> (str-alloc +ev_msg_key_size)
				(+ev_msg_type +ev_type_key_up)
				(+ev_msg_target_id old_id)
				(+ev_msg_key_scode scode)
				(+ev_msg_key_key (cook-key scode))
				(+ev_msg_key_mod *mods*))))))

(defun action-mouse-wheel ()
	(mouse-type (defq view (set-mouse-id))
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0)))
	(if (defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_wheel_size)
			(+ev_msg_type +ev_type_wheel)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_wheel_x (getf msg +sdl_mouse_wheel_event_x))
			(+ev_msg_wheel_y (getf msg +sdl_mouse_wheel_event_y))
			(+ev_msg_wheel_direction (getf msg +sdl_mouse_wheel_event_direction))))))

(defun action-mouse-motion ()
	(setq *mouse_x* (getf msg +sdl_mouse_motion_event_x)
		*mouse_y* (getf msg +sdl_mouse_motion_event_y)
		*mouse_buttons* (getf msg +sdl_mouse_motion_event_state))
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
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0))))

(defun action-mouse-button-down ()
	(setq *mouse_x* (getf msg +sdl_mouse_button_event_x)
		*mouse_y* (getf msg +sdl_mouse_button_event_y)
		*mouse_buttons* (logior *mouse_buttons* (getf msg +sdl_mouse_button_event_button)))
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
			(+ev_msg_mouse_count (getf msg +sdl_mouse_button_event_clicks))))
		(setq *focus* (if (Textfield? view) view))))

(defun action-mouse-button-up ()
	(setq *mouse_x* (getf msg +sdl_mouse_button_event_x)
		*mouse_y* (getf msg +sdl_mouse_button_event_y)
		*mouse_buttons* (logand *mouse_buttons* (lognot (getf msg +sdl_mouse_button_event_button))))
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
			(+ev_msg_mouse_count (getf msg +sdl_mouse_button_event_clicks)))))
	(mouse-type (defq view (set-mouse-id))
		(- *mouse_x* (getf view +view_ctx_x 0))
		(- *mouse_y* (getf view +view_ctx_y 0))))

(defun action-window ()
	(cond
		((= (defq event (getf msg +sdl_window_event_event)) +SDL_WINDOWEVENT_SIZE_CHANGED)
			(.-> *screen*
				(:set_bounds 0 0
					(getf msg +sdl_window_event_data1)
					(getf msg +sdl_window_event_data2))
				(:set_flags +view_flag_dirty_all +view_flag_dirty_all))
			(each (lambda (child)
				(mail-send (. child :find_owner)
					(setf-> (str-alloc +ev_msg_gui_size)
						(+ev_msg_type +ev_type_gui)
						(+ev_msg_target_id (. child :get_id))))) (. *screen* :children))
			(gui-update *mouse_x* *mouse_y* 1))
		((or (= event +SDL_WINDOWEVENT_SHOWN) (= event +SDL_WINDOWEVENT_RESTORED))
			(. *screen* :set_flags +view_flag_dirty_all +view_flag_dirty_all))))

(defun action-quit ()
	;launch logout app
	(open-child "apps/logout/app.lisp" +kn_call_open))

(defq *event_map* (Fmap-kv
	+SDL_MOUSEWHEEL action-mouse-wheel
	+SDL_MOUSEMOTION action-mouse-motion
	+SDL_MOUSEBUTTONDOWN action-mouse-button-down
	+SDL_MOUSEBUTTONUP action-mouse-button-up
	+SDL_KEYDOWN action-key-down
	+SDL_KEYUP action-key-up
	+SDL_WINDOWEVENT action-window
	+SDL_QUIT action-quit))

;module
(export-symbols
	'(*event_map*))
(env-pop)
