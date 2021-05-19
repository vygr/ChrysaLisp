;module
(env-push)

;default key bindings map
(defq key_map (xmap) key_map_control (xmap))

(defmacro binding (kmap key action)
	`(. ,kmap :insert ,key ,(eval action)))

;cursor left
(binding key_map 0x40000050 action-left)
;cursor right
(binding key_map 0x4000004f action-right)
;cursor up
(binding key_map 0x40000052 action-up)
;cursor down
(binding key_map 0x40000051 action-down)
;line break
(binding key_map +char_lf action-break)
(binding key_map +char_cr action-break)
;backspace
(binding key_map +char_backspace action-backspace)
;delete
(binding key_map +char_delete action-delete)
;tab
(binding key_map +char_tab action-tab)
;undo
(binding key_map_control (ascii-code "z") action-undo)
;redo
(binding key_map_control (ascii-code "Z") action-redo)
;save
(binding key_map_control (ascii-code "s") action-save)
;cut
(binding key_map_control (ascii-code "x") action-cut)
;copy
(binding key_map_control (ascii-code "c") action-copy)
;paste
(binding key_map_control (ascii-code "v") action-paste)
;minimise
(binding key_map_control (ascii-code "-") action-minimise)
;maximise
(binding key_map_control (ascii-code "=") action-maximise)

;module
(export (penv)
	key_map key_map_control)
(env-pop)
