(import 'class/label/class.inc)

(def-class 'title 'label)
(dec-method 'vtable 'class/class_title)
(dec-method 'create 'class/title/create)
(dec-method 'new 'class/title/new)
(dec-method 'init 'class/title/init)

(dec-method 'mouse_down 'class/title/mouse_down 'final)
(dec-method 'mouse_move 'class/title/mouse_move 'final)

(def-struct 'title 'label)
	(int 'last_x)
	(int 'last_y)
(def-struct-end)
