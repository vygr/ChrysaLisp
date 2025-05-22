;lisp bindings
(ffi "service/gui/lisp_info" gui-info)
; (gui-info) -> (mx my sw sh)
(ffi "service/gui/lisp_init" gui-init)
; (gui-init screen) -> screen
(ffi "service/gui/lisp_deinit" gui-deinit)
; (gui-deinit) -> :nil
(ffi "service/gui/lisp_update" gui-update)
; (gui-update mx my flags) -> :nil
(ffi "service/gui/lisp_event" gui-event)
; (gui-event) -> :nil | str

;gui events for Lisp level (getf)

(enums +ev_type 0
	(enum mouse key_down key_up action gui wheel enter exit))

(structure +ev_msg 0
	(ulong target_id type))

(structure +ev_msg_mouse +ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(structure +ev_msg_wheel +ev_msg_size
	(uint direction)
	(int x y))

(structure +ev_msg_key +ev_msg_size
	(uint scode key mod))

(structure +ev_msg_action +ev_msg_size
	(ulong source_id))

(structure +ev_msg_gui +ev_msg_size)

(structure +ev_msg_enter +ev_msg_size)

(structure +ev_msg_exit +ev_msg_size)

(defq +ev_key_mod_left_shift 0x1 +ev_key_mod_right_shift 0x2
	+ev_key_mod_left_control 0x40 +ev_key_mod_right_control 0x80
	+ev_key_mod_left_alt 0x100 +ev_key_mod_right_alt 0x200
	+ev_key_mod_left_meta 0x400 +ev_key_mod_right_meta 0x800
	+ev_key_mod_num_lock 0x1000 +ev_key_mod_caps_lock 0x2000
	+ev_key_mod_mode 0x4000
	+ev_key_mod_shift (+ +ev_key_mod_left_shift +ev_key_mod_right_shift)
	+ev_key_mod_control (+ +ev_key_mod_left_control +ev_key_mod_right_control)
	+ev_key_mod_alt (+ +ev_key_mod_left_alt +ev_key_mod_right_alt)
	+ev_key_mod_meta (+ +ev_key_mod_left_meta +ev_key_mod_right_meta))

;add/sub from screen

(defun gui-rpc (view cmd)
	; (gui-rpc (view cmd) -> :nil | view
	(when (nempty? (defq service (mail-enquire "Gui,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (list cmd view (task-netid) mbox))
		(mail-read mbox)
		(mail-free-mbox mbox)
		view))

(defmacro gui-quit-rpc () `(gui-rpc :nil 0))
; (gui-quit-rpc) -> :nil
(defmacro gui-logout-rpc () `(gui-rpc :nil 1))
; (gui-logout-rpc) -> :nil
(defmacro gui-sub-rpc (view) `(gui-rpc ,view 2))
; (gui-sub-rpc view) -> view
(defmacro gui-add-front-rpc (view) `(gui-rpc ,view 3))
; (gui-add-front-rpc view) -> view
(defmacro gui-add-back-rpc (view) `(gui-rpc ,view 4))
; (gui-add-back-rpc view) -> view
