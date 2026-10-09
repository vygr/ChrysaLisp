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
	(enum mouse key_down key_up action gui wheel enter exit pointer))

;the events every app has, with the same id in each. An id below 0 is that
;of a view, and the event is for that view. One from 0 up is an action of
;the app's. The first of those are the system's: what is not one of the
;app's own widgets may send them, the GUI as a user logs out or the theme
;is changed, a window manager, a test, and each means the same to every
;app. An app acts on those it has a use for, only it knows what a new
;theme or a new size is to it. Its own events start at +event_user.
;	close		the app is to end. Sent to all at logout
;	max, min	its window is to be its largest, its smallest
;	theme		the theme of the desktop is now this, the name follows
;	layout		the space it has is not what it was, lay out again.
;				Sent to all when the host window is resized
;	zoom_in, zoom_out	what it shows is to be larger, smaller
(defq +event_close 0 +event_max 1 +event_min 2 +event_theme 3 +event_layout 4
	+event_zoom_in 5 +event_zoom_out 6 +event_user 16)

(structure +ev_msg 0
	(long target_id type))

(structure +ev_msg_mouse +ev_msg_size
	(uint buttons count)
	(int x y rx ry))

;a pen or a finger, there can be many at once. id is which, kind is a
;+gui_kind, buttons those it has held, 0 when it is up or only near, as a
;mouse has them, and pressure how hard, 0 to 65535. A view that has a
;:pointer method is given it. For one that has not, the first that is down
;is made a mouse, see (. window :event)
(structure +ev_msg_pointer +ev_msg_size
	(uint id kind buttons pressure)
	(int x y rx ry))

(structure +ev_msg_wheel +ev_msg_size
	(uint direction)
	(int x y))

(structure +ev_msg_key +ev_msg_size
	(uint scode key mod))

(structure +ev_msg_action +ev_msg_size
	(long source_id))

(structure +ev_msg_gui +ev_msg_size)

;the theme of the desktop has changed, an action, its name follows
(structure +ev_msg_theme +ev_msg_action_size
	(offset name))

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

;gui rpc msgs

(enums +gui_rpc_type 0
	(enum quit logout sub add_front add_back theme))

(structure +gui_rpc 0
	(netid reply_id)
	(netid owner_id)
	(long weak_view)
	(uint type))

(defun gui-rpc (view cmd)
	; (gui-rpc (view cmd) -> :nil | view
	(when (nempty? (defq service (mail-enquire "Gui,")))
		(defq service (hex-decode (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +gui_rpc_size)
			(+gui_rpc_type cmd)
			(+gui_rpc_reply_id mbox)
			(+gui_rpc_owner_id (task-mbox))
			(+gui_rpc_weak_view (weak-ref (ifn view 0)))))
		(when (mail-read-timeout mbox)
			view)))

(defun gui-theme-rpc (name)
	; (gui-theme-rpc name) -> :nil | name
	;tell the GUI the theme of the desktop is now this one, it tells every
	;window, lib/theme/theme.inc
	(when (nempty? (defq service (mail-enquire "Gui,")))
		(defq service (hex-decode (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (cat (setf-> (str-alloc +gui_rpc_size)
			(+gui_rpc_type +gui_rpc_type_theme)
			(+gui_rpc_reply_id mbox)
			(+gui_rpc_owner_id (task-mbox))
			;no view, said as the other requests say it
			(+gui_rpc_weak_view (weak-ref 0))) name))
		(when (mail-read-timeout mbox)
			name)))

(defmacro gui-quit-rpc () `(gui-rpc :nil +gui_rpc_type_quit))
; (gui-quit-rpc) -> :nil
(defmacro gui-logout-rpc () `(gui-rpc :nil +gui_rpc_type_logout))
; (gui-logout-rpc) -> :nil
(defmacro gui-sub-rpc (view) `(gui-rpc ,view +gui_rpc_type_sub))
; (gui-sub-rpc view) -> view
(defmacro gui-add-front-rpc (view) `(gui-rpc ,view +gui_rpc_type_add_front))
; (gui-add-front-rpc view) -> view
(defmacro gui-add-back-rpc (view) `(gui-rpc ,view +gui_rpc_type_add_back))
; (gui-add-back-rpc view) -> view
