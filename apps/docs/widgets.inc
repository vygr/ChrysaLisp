(enums +event 0
	(enum close max min)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum file_tree_collapse file_tree_expand)
	(enum whole_words regexp find_down find_up))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *find_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe9cd 0xe9a8 0xe914 0xe91b) +event_whole_words))
		(. (ui-textfield *find_text* (:color +argb_white
				:hint_text "find" :clear_text "")) :connect +event_find_down))
	(ui-flow *doc_flow* (:flow_flags +flow_right_fill :color *env_toolbar_col*)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _  (:flow_flags +flow_down_fill)
				(ui-title-bar _ "Project" (0xe940 0xe941) +event_file_tree_collapse)
				(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
					(. (ui-tree *file_tree* +event_file_folder_action
							(:min_width 0 :color +argb_white
							:font *env_medium_terminal_font*))
						:connect +event_tree_action)))
			(ui-backdrop _ (:color +argb_white)))
		(ui-scroll *page_scroll* +scroll_flag_both)))

(ui-tool-tips *find_toolbar*
	'("whole words" "regexp" "find down" "find up"))
