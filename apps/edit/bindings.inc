;module
(env-push)

;default key bindings map
(defq key_map (xmap))

;cursor left
(. key_map :insert 0x40000050
	(lambda ()
		(. text_buf :cursor_left)
		(refresh)))
;cursor right
(. key_map :insert 0x4000004f
	(lambda ()
		(. text_buf :cursor_right)
		(refresh)))
;cursor up
(. key_map :insert 0x40000052
	(lambda ()
		(. text_buf :cursor_up)
		(refresh)))
;cursor down
(. key_map :insert 0x40000051
	(lambda ()
		(. text_buf :cursor_down)
		(refresh)))
;line break
(. key_map :insert 10
	(lambda ()
		(. text_buf :break)
		(refresh)))
(. key_map :insert 13
	(lambda ()
		(. text_buf :break)
		(refresh)))
;backspace
(. key_map :insert 8
	(lambda ()
		(. text_buf :backspace)
		(refresh)))
;delete
(. key_map :insert 127
	(lambda ()
		(. text_buf :delete)
		(refresh)))
;tab
(. key_map :insert 9
	(lambda ()
		(bind '(x y) (. text_buf :get_cursor))
		(defq tab_width (get :tab_width text_buf))
		(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        "))
		(refresh)))

;module
(export (penv)
	key_map)
(env-pop)
