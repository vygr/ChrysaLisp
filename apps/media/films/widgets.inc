(enums +event 0
	(enum close max min)
	(enum file_folder_action file_leaf_action))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-files *file_selector* "Films" +event_file_folder_action :nil)
		(ui-scroll *image_scroll* +scroll_flag_both)))
