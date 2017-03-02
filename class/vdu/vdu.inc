(import 'class/view/view.inc)

(def-class 'vdu 'view)
(def-method 'create 'class/vdu/create)
(def-method 'new 'class/vdu/new)
(def-method 'init 'class/vdu/init)
(def-method 'set_font 'class/vdu/set_font 'static '(r0 r1 r2))
(def-method 'set_text_color 'class/vdu/set_text_color 'static '(r0 r1))
(def-method 'print_char 'class/vdu/print_char 'static '(r0 r1))
(def-method 'print_cstr 'class/vdu/print_cstr 'static '(r0 r1))

(def-method 'deinit 'class/vdu/deinit 'override)
(def-method 'pref_size 'class/vdu/pref_size 'override)
(def-method 'draw 'class/vdu/draw 'override)

(defcvar 'vdu_default_width 60)
(defcvar 'vdu_default_height 40)

(def-struct 'vdu 'view)
	(ptr 'vdu_font)
	(pubyte 'vdu_chars)
	(pptr 'vdu_textures)
	(int 'vdu_cursor_x)
	(int 'vdu_cursor_y)
	(uint 'vdu_width)
	(uint 'vdu_height)
	(ushort 'vdu_char_width)
	(ushort 'vdu_char_height)
	(uint 'vdu_text_color)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/vdu/set_text_color ()
	;inputs
	;r0 = vdu object
	;r1 = color
	(vp-cpy-ri-i r1 r0 vdu_text_color))
