;module
(env-push)

;default key bindings map
(defq key_map (xmap))

(defmacro binding (key &rest _)
	`(. key_map :insert ,key
		(lambda () ~_ (refresh))))

;cursor left
(binding 0x40000050
	(. text_buf :cursor_left))
;cursor right
(binding 0x4000004f
	(. text_buf :cursor_right))
;cursor up
(binding 0x40000052
	(. text_buf :cursor_up))
;cursor down
(binding 0x40000051
	(. text_buf :cursor_down))
;line break
(binding 10
	(. text_buf :break))
(binding 13
	(. text_buf :break))
;backspace
(binding 8
	(. text_buf :backspace))
;delete
(binding 127
	(. text_buf :delete))
;tab
(binding 9
	(bind '(x y) (. text_buf :get_cursor))
	(defq tab_width (get :tab_width text_buf))
	(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        ")))

;module
(export (penv)
	key_map)
(env-pop)
