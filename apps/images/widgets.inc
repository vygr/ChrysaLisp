(enums +event 0
	(enum close)
	(enum file_folder_action file_leaf_action))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-files *file_selector* "Files" +event_file_folder_action :nil)
		(ui-scroll *image_scroll* +scroll_flag_both)))
