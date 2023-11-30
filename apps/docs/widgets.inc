(enums +event 0
	(enum close max min)
	(enum undo redo rewind)
	(enum file_folder_action file_leaf_action))

(ui-window *ui_window*
	(:min_width 128
	:min_height 128
	:font *env_window_font*
	:ink_color *env_ink_col*
	:color *env_window_col*
	:hint_color *env_hint_col*
	:no_hint_color *env_no_hint_col*
	:border *env_window_border*
	:shadow *env_window_shadow*)
	(ui-button _ (
		:text "Main Area"
		:min_width 128
		:min_height 128
		:border 0)))

(ui-root *ui_grid* (Flow)
	(:flow_flags +flow_right)
	(ui-grid _
		(:grid_width 3
		:grid_height 2
		:color *env_window_col*)
		(ui-button _ (:text "Cell 1"))
		(ui-button _ (:text "Cell 2"))
		(ui-button _ (:text "Cell 3"))
		(ui-button _ (:text "Cell 4"))
		(ui-button _ (:text "Cell 5"))
		(ui-button _ (:text "Cell 6"))))

(ui-root *ui_flow* (Flow)
	(:flow_flags +flow_right
	:color *env_window_col*)
	(ui-button _ (:text "Cell 1"))
	(ui-button _ (:text "Cell 2"))
	(ui-button _ (:text "Cell 3"))
	(ui-button _ (:text "Cell 4"))
	(ui-button _ (:text "Cell 5"))
	(ui-button _ (:text "Cell 6")))

(ui-root *ui_stack* (Flow)
	(:flow_flags +flow_right)
	(ui-stack _ '("Tab 1" "Tab 2" "Tab 3")
		(:color *env_window_col*
		:border *env_button_border*
		:min_height 128)
		(ui-button _ (:text "View 1"))
		(ui-button _ (:text "View 2"))
		(ui-button _ (:text "View 3"))))

(ui-root *ui_text* (Flow)
	(:flow_flags +flow_right)
	(ui-text _ (
		:offset 0
		:color 0
		:ink_color *env_ink_col*
		:font *env_window_font*
		:text "Look At Me")))

(ui-root *ui_label* (Flow)
	(:flow_flags +flow_right)
	(ui-label _ (
		:color *env_window_col*
		:ink_color *env_ink_col*
		:border *env_label_border*
		:font *env_window_font*
		:text "Look At Me")))

(ui-root *ui_button* (Flow)
	(:flow_flags +flow_right)
	(ui-button _ (
		:color *env_window_col*
		:ink_color *env_ink_col*
		:border *env_button_border*
		:font *env_window_font*
		:text "Click Me"
		:tip_text "a button")))

(ui-root *ui_spinner* (Flow)
	(:flow_flags +flow_right)
	(ui-spinner _ (
		:value 50
		:maximum 100
		:minimum 0)))

(ui-root *ui_hchart* (Flow)
	(:flow_flags +flow_right)
	(ui-hchart view "My Chart" 10
		(:color +argb_green
		:units 1024)))
(times 3 (. view :add_bar))

(ui-root *ui_title* (Flow)
	(:flow_flags +flow_right)
	(ui-title _ (
		:color *env_title_col*
		:ink_color *env_ink_col*
		:border *env_title_border*
		:font *env_title_font*
		:text "My Title")))

(ui-root *ui_title_bar* (Flow)
	(:flow_flags +flow_right)
	(ui-title-bar _ "My Title Bar" (0xea19 0xea1b 0xea1a) +event_close))

(ui-root *ui_buttons* (Flow)
	(:flow_flags +flow_right
	:font *env_toolbar_font*)
	(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo))

(ui-root *ui_toolbar* (Flow)
	(:flow_flags +flow_right)
	(ui-tool-bar view ()
		(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo)))
(ui-tool-tips view '("undo" "redo" "rewind"))

(ui-root *ui_textfield* (Flow)
	(:flow_flags +flow_right)
	(ui-textfield _
		(:offset 0
		:cursor 0
		:anchor 0
		:ink_color *env_ink_col*
		:border *env_textfield_border*
		:font *env_window_font*
		:hint_text "type somthing"
		:hint_color *env_hint_col*
		:no_hint_color *env_no_hint_col*
		:text ""
		:clear_text "")))

(ui-root *ui_slider* (Flow)
	(:flow_flags +flow_right_fill)
	(ui-slider _
		(:color *env_slider_col*
		:value 50
		:maximum 100
		:portion 25)))

(ui-root *ui_scroll* (Flow)
	(:flow_flags +flow_right)
	(ui-scroll _ +scroll_flag_both
		(:color *env_slider_col*
		:min_width 128
		:min_height 128)
		(ui-button view
			(:color +argb_orange
			:min_width 192
			:min_height 192
			:font *env_window_font*
			:text "Scroll View"))))
(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)

(ui-root *ui_backdrop* (Flow)
	(:flow_flags +flow_right)
	(ui-backdrop _
		(:color +argb_black
		:ink_color +argb_white
		:style :grid
		:spacing 8
		:min_width 128
		:min_height 128)))

(ui-root *ui_progress* (Flow)
	(:flow_flags +flow_right_fill)
	(ui-progress _
		(:color +argb_green
		:value 50
		:maximum 100)))

(ui-root *ui_vdu* (Flow)
	(:flow_flags +flow_right)
	(ui-flow _
		(:flow_flags +flow_stack_fill)
		(ui-vdu view
			(:vdu_width 16
			:vdu_height 4
			:ink_color +argb_green
			:font *env_editor_font*))
		(ui-backdrop _
			(:color +argb_grey1
			:style :plain))))
(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)
(. view :load
	'({This is line 1.}
	{"This is line 2.}
	{"This is line 3.}
	{"This is line 4.})
	0 0 0 0)

(ui-root *ui_tree* (Flow)
	(:flow_flags +flow_right)
	(ui-tree view +event_file_folder_action
		(:min_width 0
		:color 0
		:font *env_medium_terminal_font*)))
(. view :populate "apps/docs/" ".inc")
(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)
(. view :set_flags +view_flag_solid (lognot +view_flag_solid))
