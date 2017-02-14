(import 'class/view/view.inc)

(defcvar 'window_border_size 4)

(def-bit)
	(bit 'window_drag_left)
	(bit 'window_drag_right)
	(bit 'window_drag_top)
	(bit 'window_drag_bottom)
(def-bit-end)

(def-bit)
	(bit 'window_flag_status)
	(bit 'window_flag_close)
	(bit 'window_flag_max)
	(bit 'window_flag_min)
(def-bit-end)

(def-class 'window 'view)
(def-method 'new 'class/window/new)
(def-method 'create 'class/window/create 'static '(r0))
(def-method 'init 'class/window/init 'static '(r0 r1 r2))
(def-method 'set_title 'class/window/set_title 'static '(r0 r1))
(def-method 'set_status 'class/window/set_status 'static '(r0 r1))
(def-method 'get_close 'class/window/get_close 'static '(r0) '(r1))
(def-method 'get_max 'class/window/get_max 'static '(r0) '(r1))
(def-method 'get_min 'class/window/get_min 'static '(r0) '(r1))

(def-method 'deinit 'class/window/deinit 'override)
(def-method 'add_child 'class/window/add_child 'override)
(def-method 'pref_size 'class/window/pref_size 'override)
(def-method 'layout 'class/window/layout 'override)
(def-method 'draw 'class/window/draw 'override)
(def-method 'mouse_down 'class/window/mouse_down 'override)
(def-method 'mouse_move 'class/window/mouse_move 'override)

(def-struct 'window 'view)
	(offset 'window_start)
	(ptr 'window_border)
	(ptr 'window_title)
	(ptr 'window_status)
	(ptr 'window_flow)
	(ptr 'window_panel)
	(ptr 'window_close)
	(ptr 'window_max)
	(ptr 'window_min)
	(int 'window_offset_x)
	(int 'window_offset_y)
	(ulong 'window_drag_mode)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/window/get_close ()
	;inputs
	;r0 = window object
	;outputs
	;r0 = window object
	;r1 = close button object
	(vp-cpy-ir r0 window_close r1))

(defcfun class/window/get_max ()
	;inputs
	;r0 = window object
	;outputs
	;r0 = window object
	;r1 = max button object
	(vp-cpy-ir r0 window_max r1))

(defcfun class/window/get_min ()
	;inputs
	;r0 = window object
	;outputs
	;r0 = window object
	;r1 = min button object
	(vp-cpy-ir r0 window_min r1))
