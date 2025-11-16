;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_add_folder action-add-folder
	+event_add_item action-add-item
	+event_delete_node action-delete-node
	+event_rename_node action-rename-node
	+event_save_tree action-save-tree
	+event_load_tree action-load-tree
	+event_node_action action-node-action
	+event_node_leaf_action action-node-leaf-action
	+event_name_action action-name-action))

;module
(export-symbols '(*event_map*))
(env-pop)
