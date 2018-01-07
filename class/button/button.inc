(import 'class/label/label.inc)

(def-bit 'button_state)
	(bit 'pressed)
(def-bit-end)

(def-class 'button 'label)
(def-method 'create 'class/button/create)
(def-method 'new 'class/button/new)
(def-method 'init 'class/button/init)
(def-method 'connect_clicked 'class/button/connect_clicked 'static '(r0 r1) '(r0))

(def-method 'deinit 'class/button/deinit 'final)
(def-method 'draw 'class/button/draw 'final)
(def-method 'layout 'class/button/layout 'final)
(def-method 'mouse_down 'class/button/mouse_down 'final)
(def-method 'mouse_up 'class/button/mouse_up 'final)
(def-method 'mouse_move 'class/button/mouse_move 'final)

(def-struct 'button 'label)
	(ulong 'state)
	(ptr 'clicked_targets)
(def-struct-end)
