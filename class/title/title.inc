(import 'class/label/label.inc)

(def-class 'title 'label)
(def-method 'create 'class/title/create)
(def-method 'new 'class/title/new)
(def-method 'init 'class/title/init)

(def-method 'mouse_down 'class/title/mouse_down 'final)
(def-method 'mouse_move 'class/title/mouse_move 'final)

(def-struct 'title 'label)
	(int 'last_x)
	(int 'last_y)
(def-struct-end)
