(import 'class/view/view.inc)

(def-class 'vdu 'view)
(def-method 'create 'class/vdu/create 'static '() '(r0))
(def-method 'new 'class/vdu/new 'static nil '(r0))
(def-method 'init 'class/vdu/init)
(def-method 'set_font 'class/vdu/set_font 'static '(r0 r1 r2) '(r0))
(def-method 'set_text_color 'class/vdu/set_text_color 'static '(r0 r1) '(r0))
(def-method 'print_char 'class/vdu/print_char 'static '(r0 r1) '(r0))
(def-method 'print_cstr 'class/vdu/print_cstr 'static '(r0 r1) '(r0))

(def-method 'deinit 'class/vdu/deinit 'final)
(def-method 'pref_size 'class/vdu/pref_size 'final)
(def-method 'draw 'class/vdu/draw 'final)

(defcvar 'vdu_default_width 60)
(defcvar 'vdu_default_height 40)

(def-struct 'vdu 'view)
	(ptr 'font)
	(pubyte 'chars)
	(ptr 'textures)
	(int 'cursor_x)
	(int 'cursor_y)
	(uint 'width)
	(uint 'height)
	(ushort 'char_width)
	(ushort 'char_height)
	(uint 'text_color)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/vdu/set_text_color ()
	;inputs
	;r0 = vdu object
	;r1 = color
	(vp-cpy-ri-i r1 r0 vdu_text_color))
