(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum undo redo rewind gundo gredo
		cut copy paste
		split reflow paragraph tab_left tab_right
		block bracket_left bracket_right
		toupper tolower sort unique
		invert comment)
	(enum prev next scratch close_buffer close_all save save_all load_all new)
	(enum global region whole_words regexp find_down find_up)
	(enum collect collect_global replace replace_all replace_global)
	(enum macro_playback macro_to_eof macro_global macro_record)
	(enum open_tree_collapse open_tree_expand)
	(enum file_tree_collapse file_tree_expand))

(defq +vdu_min_width 40 +vdu_min_height 20 +vdu_max_width 120 +vdu_max_height 80
	+vdu_line_width 5 +status_min_size 32)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Edit" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xe938 0xe971
				0xea08 0xe9c9 0xe9ca
				0xe901 0xe909 0xe90d 0xe90a 0xe90b
				0xe955 0xe93c 0xe93d
				0xea36 0xea33 0xea27 0xea28
				0xea26 0xe9c4) +event_undo))
		(ui-tool-bar *macro_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (0xe95c 0xe95e 0xe95a 0xe95f) +event_macro_playback))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *buffer_toolbar* (:color (get :color *macro_toolbar*))
			(ui-buttons (0xe91d 0xe91e 0xe94b 0xe94d 0xe94c 0xea07 0xe97e 0xea3d 0xe94e) +event_prev))
		(. (ui-textfield *name_text* (:color +argb_white
				:hint_text "new file" :clear_text "")) :connect +event_new))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_height 1)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *find_toolbar* (:color (get :color *macro_toolbar*))
					(ui-buttons (0xe9a1 0xe962 0xe9cd 0xe9a8 0xe914 0xe91b) +event_global))
				(. (ui-textfield *find_text* (:color +argb_white
						:hint_text "find" :clear_text "")) :connect +event_find_down))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *replace_toolbar* (:color (get :color *macro_toolbar*))
					(ui-buttons (0xe95b 0xe959 0xe95c 0xe95e 0xe95a) +event_collect))
				(. (ui-textfield *replace_text* (:color +argb_white
						:hint_text "replace" :clear_text "")) :connect +event_replace))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-grid _ (:color +argb_grey14 :grid_width 1)
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-title-bar _ "Open" (0xe940 0xe941) +event_open_tree_collapse)
					(ui-scroll *open_tree_scroll* +scroll_flag_vertical :nil
						(. (ui-tree *open_tree* +event_open_folder_action
								(:min_width 0 :color +argb_white
								:font *env_medium_terminal_font*)) :connect +event_tree_action)))
				(ui-flow _  (:flow_flags +flow_down_fill)
					(ui-title-bar _ "Project" (0xe940 0xe941) +event_file_tree_collapse)
					(ui-scroll *file_tree_scroll* +scroll_flag_vertical :nil
						(. (ui-tree *file_tree* +event_file_folder_action
								(:min_width 0 :color (get :color *open_tree*)
								:font (get :font *open_tree*))) :connect +event_tree_action))))
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
	'("undo" "redo" "rewind" "global undo" "global redo"
	"cut" "copy" "paste" "split" "reflow" "select paragraph"
	"outdent" "indent" "select form" "start form"
	"end form" "upper case" "lower case" "sort" "unique"
	"reverse" "comment"))

(ui-tool-tips *buffer_toolbar*
	'("previous" "next" "scratchpad" "close" "close all"
	"save" "save all" "load all" "new"))

(ui-tool-tips *find_toolbar*
	'("global search" "select region" "whole words"
	"regexp" "find down" "find up"))

(ui-tool-tips *macro_toolbar*
	'("playback" "playback eof" "playback global" "record"))

(ui-tool-tips *replace_toolbar*
	'("collect" "collect global" "replace" "replace all"
	"replace global"))
