(enums +event 0
	(enum close)
	(enum add_folder add_item delete_node rename_node)
	(enum save_tree load_tree)
	(enum node_action node_leaf_action)
	(enum name_action))

(ui-window *window* :nil
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar *window_title* "" (0xea19) +event_close)
		; Toolbar with action buttons
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-buttons (0xe145) +event_add_folder (:font *env_toolbar_font*)) ; folder icon
			(ui-buttons (0xe24d) +event_add_item (:font *env_toolbar_font*)) ; insert drive file icon
			(ui-buttons (0xe872) +event_delete_node (:font *env_toolbar_font*)) ; delete icon
			(ui-buttons (0xe3c9) +event_rename_node (:font *env_toolbar_font*)) ; edit icon
			(ui-buttons (0xe161) +event_save_tree (:font *env_toolbar_font*)) ; save icon
			(ui-buttons (0xe2c4) +event_load_tree (:font *env_toolbar_font*))) ; folder open icon
		; Node name input
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-label _ (:text "Node Name:"))
			(. (ui-textfield *node_name* (:clear_text "" :hint_text "Enter node name" :color +argb_white))
				:connect +event_name_action))
		; Tree display with scroll
		(ui-files *tree_view* "Tree Structure" +event_node_action :nil)))
