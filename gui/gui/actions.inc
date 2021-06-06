;module
(env-push)

(defun cook_key (key mod)
	(and (/= (logand mod (const (logior +ev_key_mod_caps_lock +ev_key_mod_shift))) 0)
		(defq i (find (char key) "§1234567890-=qwertyuiop[]asdfghjkl;\`zxcvbnm,./'"))
		(setq key (code (elem i (const (cat "±!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:|~ZXCVBNM<>?" (char 34)))))))
	key)

(defun action-mouse-wheel ()
	(bind '(view _ _) (. *screen* :hit_tree *mouse_x* *mouse_y*))
	(setq *mouse_id* (. view :get_id))
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
	(when (= *mouse_buttons* 0)
		(bind '(view _ _) (. *screen* :hit_tree *mouse_x* *mouse_y*))
		(setq *mouse_id* (. view :get_id)))
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
			(+ev_msg_mouse_count 0)))))

(defun action-mouse-button-down ()
	(setq *mouse_x* (getf msg +sdl_mouse_button_event_x)
		*mouse_y* (getf msg +sdl_mouse_button_event_y)
		*mouse_buttons* (logior *mouse_buttons* (getf msg +sdl_mouse_button_event_button)))
	(bind '(view _ _) (. *screen* :hit_tree *mouse_x* *mouse_y*))
	(setq *mouse_id* (. view :get_id))
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
			(+ev_msg_mouse_count (getf msg +sdl_mouse_button_event_clicks))))))

(defun action-mouse-button-up ()
	(setq *mouse_x* (getf msg +sdl_mouse_button_event_x)
		*mouse_y* (getf msg +sdl_mouse_button_event_y)
		*mouse_buttons* (logxor *mouse_buttons* (getf msg +sdl_mouse_button_event_button)))
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
			(+ev_msg_mouse_count (getf msg +sdl_mouse_button_event_clicks))))))

(defun action-key-down ()
	(defq key_code (getf msg +sdl_keyboard_event_scancode)
		key (getf msg +sdl_keyboard_event_sym)
		mod (getf msg +sdl_keyboard_event_mod))
	(and (defq view (. *screen* :find_id *mouse_id*))
		(defq owner (. view :find_owner))
		(mail-send owner (setf-> (str-alloc +ev_msg_key_size)
			(+ev_msg_type +ev_type_key)
			(+ev_msg_target_id *mouse_id*)
			(+ev_msg_key_keycode key_code)
			(+ev_msg_key_key (cook_key key mod))
			(+ev_msg_key_mod mod)))))

(defun action-window ()
	(defq event (getf msg +sdl_window_event_event))
	(cond
		((= event +SDL_WINDOWEVENT_SIZE_CHANGED)
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
	;nil soon...
	(setq id t))

(defq
event_map (xmap-kv
	+SDL_MOUSEWHEEL action-mouse-wheel
	+SDL_MOUSEMOTION action-mouse-motion
	+SDL_MOUSEBUTTONDOWN action-mouse-button-down
	+SDL_MOUSEBUTTONUP action-mouse-button-up
	+SDL_KEYDOWN action-key-down
	+SDL_WINDOWEVENT action-window
	+SDL_QUIT action-quit))

;module
(export (penv)
	event_map)
(env-pop)
