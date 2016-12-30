(import 'class/label/label.inc)

(defcvar 'button_border_size 2)

(def-bit)
	(bit 'button_state_pressed)
(def-bit-end)

(def-class 'button 'label)
(def-method 'create 'class/button/create)
(def-method 'new 'class/button/new)
(def-method 'init 'class/button/init)
(def-method 'sig_pressed 'class/button/sig_pressed 'static '(r0) '(r1))

(def-method 'deinit 'class/button/deinit 'override)
(def-method 'pref_size 'class/button/pref_size 'override)
(def-method 'draw 'class/button/draw 'override)
(def-method 'layout 'class/button/layout 'override)
(def-method 'mouse_down 'class/button/mouse_down 'override)
(def-method 'mouse_up 'class/button/mouse_up 'override)
(def-method 'mouse_move 'class/button/mouse_move 'override)

(def-struct 'button 'label)
	(ulong 'button_state)
	(struct 'button_pressed_signal 'lh_list)
(def-struct-end)
