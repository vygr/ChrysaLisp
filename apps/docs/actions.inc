;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")
(import "./search.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_whole_words action-whole-words
	+event_regexp action-regexp
	+event_next action-next-doc
	+event_prev action-prev-doc
	+event_find_down action-find-down
	+event_find_up action-find-up
	+event_tree_action action-tree-action
	+event_file_tree_expand action-file-tree-expand
	+event_file_tree_collapse action-file-tree-collapse
	+event_file_leaf_action action-file-leaf-action)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "n") action-next-doc
	(ascii-code "N") action-prev-doc
	(ascii-code "{") action-scale-down
	(ascii-code "}") action-scale-up
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
