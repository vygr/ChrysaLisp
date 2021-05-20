;default key bindings maps

(defq key_map (xmap-kv
		0x40000050 action-left
		0x4000004f action-right
		0x40000052 action-up
		0x40000051 action-down
		+char_lf action-break
		+char_cr action-break
		+char_backspace action-backspace
		+char_delete action-delete
		+char_tab action-tab)
	key_map_control (xmap-kv
		(ascii-code "z") action-undo
		(ascii-code "Z") action-redo
		(ascii-code "s") action-save
		(ascii-code "x") action-cut
		(ascii-code "c") action-copy
		(ascii-code "v") action-paste
		(ascii-code "-") action-minimise
		(ascii-code "=") action-maximise))
