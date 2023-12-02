(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum copy paragraph block bracket_left bracket_right)
	(enum region whole_words regexp find_down find_up)
	(enum file_tree_collapse file_tree_expand))

(defq +vdu_min_width 40 +vdu_min_height 20 +vdu_max_width 100
	+vdu_max_height 46 +vdu_line_width 5 +status_min_size 32)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Viewer" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9c9 0xe90d 0xe955 0xe93c 0xe93d) +event_copy))
		(ui-tool-bar *find_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe962 0xe9cd 0xe9a8 0xe914 0xe91b) +event_region))
		(. (ui-textfield *find_text* (:color +argb_white
				:hint_text "find" :clear_text "")) :connect +event_find_down))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-flow _  (:flow_flags +flow_down_fill :color +argb_grey14)
				(ui-flow _ (:flow_flags +flow_left_fill)
					(ui-buttons (">" "^") +event_file_tree_collapse)
					(ui-label _ (:text "Project")))
				(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
					(. (ui-tree *file_tree* +event_file_folder_action
							(:min_width 0 :color +argb_white
							:font *env_medium_terminal_font*)) :connect +event_tree_action)))
			(ui-backdrop _ (:color +argb_white)))
		(ui-flow _ (:flow_flags +flow_up_fill)
			(ui-flow _ (:flow_flags +flow_stack_fill :color +argb_white
					:font *env_medium_terminal_font*)
				(ui-flow _ (:flow_flags +flow_right)
					(ui-text _ (:text "cx: ")) (ui-text *cx* (:min_width +status_min_size))
					(ui-text _ (:text "cy: ")) (ui-text *cy* (:min_width +status_min_size))
					(ui-text _ (:text "sw: ")) (ui-text *sw* (:min_width +status_min_size))
					(ui-text _ (:text "sh: ")) (ui-text *sh* (:min_width +status_min_size))
					(ui-text _ (:text "fc: ")) (ui-text *fc* (:min_width +status_min_size)))
				(ui-backdrop _))
			(ui-flow *scale_flow* (:flow_flags +flow_right_fill)
				(ui-vdu *vdu_lines* (:min_width +vdu_line_width :min_height 0
						:vdu_width +vdu_line_width :vdu_height +vdu_min_height
						:ink_color +argb_grey12 :font *env_editor_font*))
				(ui-backdrop _ (:color (get :ink_color *vdu_lines*) :min_width 1))
				(ui-flow _ (:flow_flags +flow_left_fill)
					(. (ui-slider *yslider*) :connect +event_yscroll)
					(ui-flow *edit_flow* (:flow_flags +flow_up_fill)
						(. (ui-slider *xslider*) :connect +event_xscroll)
						(ui-backdrop _ (:style :lines :ink_color +argb_white))))))))

(ui-tool-tips *main_toolbar*
	'("copy" "select paragraph" "select form" "start form" "end form"))

(ui-tool-tips *find_toolbar*
	'("select region" "whole words" "regexp" "find down" "find up"))
