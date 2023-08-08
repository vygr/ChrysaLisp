;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
event_map (fmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_tree_action action-tree-action
	+event_file_leaf_action action-file-leaf-action))

;module
(export-symbols
	event_map )
(env-pop)
