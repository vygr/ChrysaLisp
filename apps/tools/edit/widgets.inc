(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum undo redo rewind
		cut copy paste
		split reflow paragraph tab_left tab_right
		block bracket_left bracket_right
		toupper tolower sort unique
		invert comment)
	(enum prev next scratch close_buffer close_all
		save save_all load_selected load_all_depends new)
	(enum global region whole_words regexp ignore_case find_down find_up)
	(enum collect collect_global replace replace_all replace_global)
	(enum macro_playback macro_to_eof macro_global macro_record)
	(enum debug_launch debug_toggle debug_remove
		debug_enable_all debug_disable_all debug_remove_all))

(defq +vdu_min_width 40 +vdu_min_height 20 +vdu_max_width 120 +vdu_max_height 80
	+vdu_line_width 5 +status_min_size 48)

(ui-window *window* (:color +argb_grey1)
	(ui-title-bar *title* "Edit" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_undo +sym_redo +sym_rewind
				+sym_cut +sym_copy +sym_paste
				+sym_split +sym_reflow +sym_select_paragraph +sym_outdent +sym_indent
				+sym_select_form +sym_start_form +sym_end_form
				+sym_upper_case +sym_lower_case +sym_sort +sym_unique
				+sym_reverse +sym_comment) +event_undo))
		(ui-tool-bar *macro_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (+sym_play +sym_play_end +sym_play_global +sym_record) +event_macro_playback))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_left_fill)
		(ui-tool-bar *debug_toolbar* (:color (const *env_toolbar2_col*))
			(ui-buttons (+sym_save_debug +sym_breakpoint +sym_breakpoint_remove +sym_enable_all +sym_disable_all +sym_remove_all) +event_debug_launch))
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *buffer_toolbar* (:color (get :color *macro_toolbar*))
				(ui-buttons (+sym_prev +sym_next +sym_scratchpad +sym_close_file +sym_close_all +sym_save +sym_save_all +sym_load +sym_load_all +sym_new) +event_prev))
			(. (ui-textfield *name_text* (:color +argb_white
					:hint_text "new file" :clear_text "")) :connect +event_new)))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:grid_height 1)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *find_toolbar* (:color (get :color *macro_toolbar*))
					(ui-buttons (+sym_find_global +sym_select_region +sym_whole_words +sym_regexp +sym_ignore_case +sym_find_down +sym_find_up) +event_global))
				(. (ui-textfield *find_text* (:color +argb_white
						:hint_text "find" :clear_text "")) :connect +event_find_down))
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *replace_toolbar* (:color (get :color *macro_toolbar*))
					(ui-buttons (+sym_collect +sym_collect_global +sym_replace +sym_replace_all +sym_replace_global) +event_collect))
				(. (ui-textfield *replace_text* (:color +argb_white
						:hint_text "replace" :clear_text "")) :connect +event_replace))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-grid _ (:color +argb_grey14 :grid_width 1)
			(ui-files *open_files_selector* "Open" +event_open_folder_action :nil)
			(ui-files *file_selector* "Project" +event_file_folder_action :nil))
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
	'("undo" "redo" "rewind"
	"cut" "copy" "paste" "split" "reflow" "select paragraph"
	"outdent" "indent" "select form" "start form"
	"end form" "upper case" "lower case" "sort" "unique"
	"reverse" "comment"))

(ui-tool-tips *buffer_toolbar*
	'("previous" "next" "scratchpad" "close" "close all"
	"save" "save all" "load selected" "load all depends" "new"))

(ui-tool-tips *find_toolbar*
	'("global search" "select region" "whole words"
	"regexp" "ignore case" "find down" "find up"))

(ui-tool-tips *macro_toolbar*
	'("playback" "playback eof" "playback global" "record"))

(ui-tool-tips *replace_toolbar*
	'("collect" "collect global" "replace" "replace all"
	"replace global"))

(ui-tool-tips *debug_toolbar*
	'("save all and debug" "toggle breakpoint" "remove breakpoint"
	"enable all" "disable all" "remove all"))