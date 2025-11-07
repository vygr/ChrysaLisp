;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_layout action-layout
	+event_todo_done action-todo-done
	+event_todo_del action-todo-del
	+event_done_redo action-done-redo
	+event_done_del action-done-del
	+event_del_redo action-del-redo
	+event_del_del action-del-del
	+event_new_item action-new-item
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
