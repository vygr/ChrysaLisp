;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_tree_action action-tree-action
	+event_file_tree_expand action-file-tree-expand
	+event_file_tree_collapse action-file-tree-collapse
	+event_file_leaf_action action-file-leaf-action)

*key_map* (Fmap-kv)

*key_map_shift* (Fmap-kv)

*key_map_control* (Fmap-kv
	(ascii-code "]") action-scale-up
	(ascii-code "[") action-scale-down
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	*event_map* *key_map* *key_map_shift* *key_map_control*)
(env-pop)
