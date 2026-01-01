;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_ok_action action-ok
	+event_exts_action action-exts
	+event_file_action action-file-action
	+event_file_leaf_action action-file-leaf-action))

;module
(export-symbols '(*event_map*))
(env-pop)
