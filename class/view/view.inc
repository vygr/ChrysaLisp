(import 'class/component/component.inc)

(def-class 'view 'component)
(def-method 'init 'class/view/init)
(def-method 'add_front 'class/view/add_front 'static '(r0 r1) '(r0))
(def-method 'add_back 'class/view/add_back 'static '(r0 r1) '(r0))
(def-method 'sub 'class/view/sub 'static '(r0) '(r0))
(def-method 'to_front 'class/view/to_front 'static '(r0) '(r0))
(def-method 'add_dirty 'class/view/add_dirty 'static '(r0 r7 r8 r9 r10) '(r0))
(def-method 'add_opaque 'class/view/add_opaque 'static '(r0 r7 r8 r9 r10) '(r0))
(def-method 'sub_opaque 'class/view/sub_opaque 'static '(r0 r7 r8 r9 r10) '(r0))
(def-method 'get_relative 'class/view/get_relative 'static '(r0 r1 r7 r8) '(r0 r7 r8))
(def-method 'dirty 'class/view/dirty 'static '(r0) '(r0))
(def-method 'dirty_all 'class/view/dirty_all 'static '(r0) '(r0))
(def-method 'opaque 'class/view/opaque 'static '(r0) '(r0))
(def-method 'forward 'class/view/forward 'static '(r0 r1 r2) '(r0))
(def-method 'backward 'class/view/backward 'static '(r0 r1 r2) '(r0))
(def-method 'forward_tree 'class/view/forward_tree 'static '(r0 r1 r2 r3) '(r0))
(def-method 'backward_tree 'class/view/backward_tree 'static '(r0 r1 r2 r3) '(r0))
(def-method 'change 'class/view/change 'static '(r0 r7 r8 r9 r10) '(r0))
(def-method 'hit_tree 'class/view/hit_tree 'static '(r0 r7 r8) '(r0 r1 r7 r8))
(def-method 'set_owner 'class/view/set_owner 'static '(r0 r1) '(r0))
(def-method 'find_owner 'class/view/find_owner 'static '(r0) '(r0 r1))
(def-method 'draw_panel 'class/view/draw_panel 'static '(r0 r1 r2 r3) '(r0))
(def-method 'get_bounds 'class/view/get_bounds 'static '(r0) '(r0 r7 r8 r9 r10))
(def-method 'set_bounds 'class/view/set_bounds 'static '(r0 r7 r8 r9 r10) '(r0))
(def-method 'set_flags 'class/view/set_flags 'static '(r0 r1) '(r0))
(def-method 'get_first 'class/view/get_first 'static '(r0) '(r0 r1))
(def-method 'get_last 'class/view/get_last 'static '(r0) '(r0 r1))
(def-method 'set_min_size 'class/view/set_min_size 'static '(r0 r9 r10) '(r0))

(def-method 'deinit 'class/view/deinit 'override)

(def-method 'add_child 'class/view/add_back 'virtual '(r0 r1) '(r0))
(def-method 'draw 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'hit 'class/view/hit 'virtual '(r0 r7 r8) '(r0 r1))
(def-method 'pref_size 'class/view/pref_size 'virtual '(r0) '(r0 r9 r10))
(def-method 'layout 'class/obj/null 'virtual '(r0) '(r0))
(def-method 'event 'class/view/event 'virtual '(r0 r1) '(r0))
(def-method 'mouse_down 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'mouse_up 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'mouse_move 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'mouse_hover 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'key_down 'class/obj/null 'virtual '(r0 r1) '(r0))
(def-method 'key_up 'class/obj/null 'virtual '(r0 r1) '(r0))

(def-bit 'view_flag)
	(bit 'solid)
(def-bit-end)

(def-struct 'view 'component)
	(offset 'start)
	(struct 'node 'ln_node)
	(struct 'list 'lh_list)
	(ptr 'tcb)
	(ptr 'dirty_region)
	(ptr 'opaque_region)
	(ptr 'ctx_node)
	(uint 'last_buttons)
	(uint 'flags)
	(int 'ctx_x)
	(int 'ctx_y)
	(int 'x)
	(int 'y)
	(int 'w)
	(int 'h)
	(int 'min_w)
	(int 'min_h)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/view/set_flags ()
	;inputs
	;r0 = view object
	;r1 = flags
	(vp-cpy-ri-i r1 r0 view_flags))

(defcfun class/view/set_owner ()
	;inputs
	;r0 = view object
	;r1 = task tcb
	(vp-cpy-ri r1 r0 view_tcb))

(defcfun class/view/set_min_size ()
	;inputs
	;r0 = view object
	;r9 = preferred width
	;r10 = preferred height
	(vp-cpy-ri-i r9 r0 view_min_w)
	(vp-cpy-ri-i r10 r0 view_min_h))
