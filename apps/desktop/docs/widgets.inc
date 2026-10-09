(enums +event +event_user
	(enum file_folder_action file_leaf_action)
	(enum prev next)
	(enum whole_words regexp ignore_case find_down find_up)
	(enum link)
	(enum back forward))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *history_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (+sym_go_back +sym_go_forward) +event_back))
		(ui-tool-bar *buffer_toolbar* :nil
			(ui-buttons (+sym_prev +sym_next) +event_prev))
		(ui-tool-bar *find_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (+sym_whole_words +sym_regexp +sym_ignore_case +sym_find_down +sym_find_up) +event_whole_words))
		(. (ui-textfield *find_text* (:color +argb_white
				:hint_text "find" :clear_text "")) :connect +event_find_down))
	(ui-flow *doc_flow* (:flow_flags +flow_right_fill :color *env_toolbar_col*)
		(ui-files *file_selector* "Project" +event_file_folder_action :nil)
		(ui-scroll *page_scroll* +scroll_flag_both)))

(ui-tool-tips *find_toolbar*
	'("whole words" "regexp" "ignore case" "find down" "find up"))

(ui-tool-tips *history_toolbar*
	'("back" "forward"))

(ui-tool-tips *buffer_toolbar*
	'("previous found" "next found"))