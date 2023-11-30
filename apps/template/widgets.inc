(enums +event 0
	(enum close max min)
	(enum undo redo rewind cut copy paste)
	(enum button_1 button_2 button_3 button_4))

(ui-window *window* ()
	(ui-title-bar *title* "Template" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xea08 0xe9c9 0xe9ca) +event_undo))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-stack *stack_flow* '("main" "settings" "status" "info")
			(:color +argb_black)
		(ui-grid *main_widget* (:grid_width 2 :grid_height 2 :color +argb_orange)
			(. (ui-button _ (:text "1")) :connect +event_button_1)
			(. (ui-button _ (:text "2")) :connect +event_button_2)
			(. (ui-button _ (:text "3")) :connect +event_button_3)
			(. (ui-button _ (:text "4")) :connect +event_button_4))
		(ui-backdrop *settings_widget* (:min_width 512 :min_height 256
				:ink_color +argb_red :spacing 16 :style :lines))
		(ui-backdrop *status_widget* (:ink_color +argb_green
				:spacing 16 :style :axis))
		(ui-backdrop *info_widget* (:ink_color +argb_blue
				:spacing 16 :style :grid))))

(ui-tool-tips *main_toolbar*
	'("undo" "redo" "rewind" "cut" "copy" "paste"))
