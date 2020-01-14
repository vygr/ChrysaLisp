(include 'gui/view/class.inc)

(def-class 'vdu 'view)
(dec-method 'vtable 'gui/vdu/vtable)
(dec-method 'create 'gui/vdu/create 'static '() '(r0))
(dec-method 'init 'gui/vdu/init)
(dec-method 'switch_font 'gui/vdu/switch_font 'static '(r0) '(r0))
(dec-method 'switch_size 'gui/vdu/switch_size 'static '(r0) '(r0))
(dec-method 'print_char 'gui/vdu/print_char 'static '(r0 r1) '(r0))
(dec-method 'print_cstr 'gui/vdu/print_cstr 'static '(r0 r1) '(r0))

(dec-method 'lisp_create 'gui/vdu/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_print 'gui/vdu/lisp_print 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_load 'gui/vdu/lisp_load 'static '(r0 r1) '(r0 r1))

(dec-method 'deinit 'gui/vdu/deinit 'final)
(dec-method 'pref_size 'gui/vdu/pref_size 'final)
(dec-method 'draw 'gui/vdu/draw 'final)

(def-struct 'vdu 'view)
	(ptr 'font 'textures)
	(pubyte 'chars)
	(int 'cursor_x 'cursor_y)
	(uint 'width 'height)
	(ushort 'char_width 'char_height)
(def-struct-end)

(defcvar 'vdu_clear_screen 128)
(defcvar 'vdu_clear_line 129)
