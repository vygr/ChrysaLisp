;module
(env-push)

;default key bindings map
(defq key_map (xmap) key_map_control (xmap))

(defmacro binding (kmap key &rest _)
	`(. ,kmap :insert ,key (lambda () ~_)))

;cursor left
(binding key_map 0x40000050
	(. text_buf :left))
;cursor right
(binding key_map 0x4000004f
	(. text_buf :right))
;cursor up
(binding key_map 0x40000052
	(. text_buf :up))
;cursor down
(binding key_map 0x40000051
	(. text_buf :down))
;line break
(binding key_map +char_lf
	(. text_buf :break))
(binding key_map +char_cr
	(. text_buf :break))
;backspace
(binding key_map +char_backspace
	(. text_buf :backspace))
;delete
(binding key_map +char_delete
	(. text_buf :delete))
;tab
(binding key_map +char_tab
	(bind '(x y) (. text_buf :get_cursor))
	(defq tab_width (get :tab_width text_buf))
	(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        ")))

;undo
(binding key_map_control (ascii-code "z")
	(. text_buf :undo))
;redo
(binding key_map_control (ascii-code "Z")
	(. text_buf :redo))
;save
(binding key_map_control (ascii-code "s")
	(. text_buf :file_save current_file))
;cut
(binding key_map_control (ascii-code "x")
	(clipboard-put (. text_buf :cut)))
;copy
(binding key_map_control (ascii-code "c")
	(clipboard-put (. text_buf :copy)))
;paste
(binding key_map_control (ascii-code "v")
	(unless (eql (defq data (clipboard-get)) "")
		(. text_buf :paste data)))

;module
(export (penv)
	key_map key_map_control)
(env-pop)
