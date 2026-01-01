(enums +event 0
	(enum close max min)
	(enum file_folder_action file_leaf_action)
	(enum prev next)
	(enum whole_words regexp find_down find_up))

(ui-window *window* (:color +argb_grey15)
	(ui-title-bar _ "Docs" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *buffer_toolbar* :nil
			(ui-buttons (0xe91d 0xe91e) +event_prev))
		(ui-tool-bar *find_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe9cd 0xe9a8 0xe914 0xe91b) +event_whole_words))
		(. (ui-textfield *find_text* (:color +argb_white
				:hint_text "find" :clear_text "")) :connect +event_find_down))
	(ui-flow *doc_flow* (:flow_flags +flow_right_fill :color *env_toolbar_col*)
		(ui-files *file_selector* "Project" +event_file_folder_action :nil)
		(ui-scroll *page_scroll* +scroll_flag_both)))

(ui-tool-tips *find_toolbar*
	'("whole words" "regexp" "find down" "find up"))

(ui-tool-tips *buffer_toolbar*
	'("previous" "next"))
