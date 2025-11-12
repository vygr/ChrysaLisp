(enums +event 0
	(enum close)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum file_tree_collapse file_tree_expand))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _  (:flow_flags +flow_down_fill :color +argb_grey14)
				(ui-title-bar _ "Project" (0xe940 0xe941) +event_file_tree_collapse)
				(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
					(. (ui-tree *file_tree* +event_file_folder_action
							(:min_width 0 :color +argb_white
							:font *env_medium_terminal_font*)) :connect +event_tree_action)))
			(ui-backdrop _ (:color +argb_white)))
		(ui-scroll *image_scroll* +scroll_flag_both)))
