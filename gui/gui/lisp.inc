;lisp bindings
(ffi gui-info "gui/gui/lisp_info" 0)
(ffi gui-init "gui/gui/lisp_init" 0)
(ffi gui-deinit "gui/gui/lisp_deinit" 0)
(ffi gui-update "gui/gui/lisp_update" 0)
(ffi gui-event "gui/gui/lisp_event" 0)

;gui events for Lisp level (getf)

(enums +ev_type 0
	(enum mouse key action gui wheel enter exit))

(structure +ev_msg 0
	(ulong target_id type))

(structure +ev_msg_mouse +ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(structure +ev_msg_wheel +ev_msg_size
	(uint direction)
	(int x y))

(structure +ev_msg_key +ev_msg_size
	(uint keycode key mod))

(structure +ev_msg_action +ev_msg_size
	(ulong source_id))

(structure +ev_msg_gui +ev_msg_size)

(structure +ev_msg_enter +ev_msg_size)

(structure +ev_msg_exit +ev_msg_size)

(defq +ev_key_mod_left_shift 0x1 +ev_key_mod_right_shift 0x2
	+ev_key_mod_left_option 0x100 +ev_key_mod_right_option 0x200
	+ev_key_mod_left_command 0x400 +ev_key_mod_right_command 0x800
	+ev_key_mod_caps_lock 0x2000
	+ev_key_mod_control 0x40
	+ev_key_mod_shift (+ +ev_key_mod_left_shift +ev_key_mod_right_shift)
	+ev_key_mod_option (+ +ev_key_mod_left_option +ev_key_mod_right_option)
	+ev_key_mod_command (+ +ev_key_mod_left_command +ev_key_mod_right_command))

;add/sub from screen

(defun gui-rpc (view cmd)
	(when (/= 0 (length (defq services (mail-enquire "GUI_SERVICE"))))
		(setq services (filter (# (eql
			(slice +long_size -1 (task-mailbox))
			(slice +long_size -1 %0))) (map (# (to-net-id (elem 1 (split %0 ",")))) services)))
		(defq mbox (mail-alloc-mbox))
		(mail-send (pop services) (list cmd view (task-mailbox) mbox))
		(mail-read mbox)
		(mail-free-mbox mbox)
		view))

(defmacro gui-sub (view) `(gui-rpc ,view 0))
(defmacro gui-add-front (view) `(gui-rpc ,view 1))
(defmacro gui-add-back (view) `(gui-rpc ,view 2))
