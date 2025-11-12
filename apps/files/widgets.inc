(enums +event 0
	(enum close)
	(enum exts_action ok_action)
	(enum file_action file_leaf_action))

(ui-window *window* :nil
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar *window_title* "" (0xea19) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-buttons (0xe93a) +event_ok_action (:font *env_toolbar_font*))
			(ui-label _ (:text "Filename:"))
			(. (ui-textfield *filename* (:clear_text "" :hint_text "filename" :color +argb_white))
				:connect +event_ok_action))
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-label _ (:text "Filter:"))
			(. (ui-textfield *exts* (:clear_text "" :hint_text "exts" :color +argb_white))
				:connect +event_exts_action))
		(ui-files *file_selector* "Files" +event_file_action :nil)))
