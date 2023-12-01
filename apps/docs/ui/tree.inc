(enums +event 0
	(enum file_folder_action file_leaf_action))

(ui-window *ui_tree*
	(:min_width 128
	:min_height 128)
	(ui-tree view +event_file_folder_action
		(:min_width 0
		:color 0
		:font *env_medium_terminal_font*)))

(. view :populate "apps/docs/ui/" ".inc")
(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)
