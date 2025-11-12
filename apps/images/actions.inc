;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_tree_action action-tree-action
	+event_file_tree_expand action-file-tree-expand
	+event_file_tree_collapse action-file-tree-collapse
	+event_file_leaf_action action-file-leaf-action)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
