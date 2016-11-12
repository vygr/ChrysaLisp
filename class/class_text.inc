%ifndef CLASS_TEXT_1234
%define CLASS_TEXT_1234

(import 'class/class_view.inc)

(def-class 'text 'view)
(def-method 'create 'class/text/create)
(def-method 'new 'class/text/new)
(def-method 'init 'class/text/init)
(def-method 'set_font 'class/text/set_font 'static '(r0, r1, r2))
(def-method 'set_text 'class/text/set_text 'static '(r0, r1))
(def-method 'get_text 'class/text/get_text 'static '(r0), '(r1))
(def-method 'set_text_color 'class/text/set_text_color 'static '(r0, r1))

(def-method 'deinit 'class/text/deinit 'override)
(def-method 'pref_size 'class/text/pref_size 'override)
(def-method 'draw 'class/text/draw 'override)

(def-struct 'text 'view)
	ptr text_font
	ptr text_string
	ulong text_text_color
	ptr text_words
(def-struct-end)

%endif
