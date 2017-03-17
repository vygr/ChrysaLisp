(import 'class/label/label.inc)

(defcvar 'title_border_size 1)

(def-class 'title 'label)
(def-method 'create 'class/title/create)
(def-method 'new 'class/title/new)

(def-method 'pref_size 'class/title/pref_size 'final)
(def-method 'draw 'class/title/draw 'final)
(def-method 'layout 'class/title/layout 'final)
(def-method 'mouse_down 'class/title/mouse_down 'final)
(def-method 'mouse_move 'class/title/mouse_move 'final)

(def-struct 'title 'label)
	(int 'title_last_x)
	(int 'title_last_y)
(def-struct-end)
