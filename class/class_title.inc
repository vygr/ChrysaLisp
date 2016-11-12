%ifndef CLASS_TITLE_1234
%define CLASS_TITLE_1234

(import 'class/class_label.inc)

title_border_size equ 1

(def-class 'title 'label)
(def-method 'create 'class/title/create)
(def-method 'new 'class/title/new)

(def-method 'pref_size 'class/title/pref_size 'override)
(def-method 'draw 'class/title/draw 'override)
(def-method 'layout 'class/title/layout 'override)
(def-method 'mouse_down 'class/title/mouse_down 'override)
(def-method 'mouse_move 'class/title/mouse_move 'override)

(def-struct 'title 'label)
	(long 'title_last_x)
	(long 'title_last_y)
(def-struct-end)

%endif
