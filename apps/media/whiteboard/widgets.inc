(import "./view.inc")
(import "lib/cwb/tools.inc")

(enums +event +event_user
	(enum undo redo new load save clear crop)
	(enum style)
	(enum snap snap_angle keep_shape)
	(enum radius ink)
	(enum mode)
	(enum ruler protractor set_square full_protractor)
	(enum group ungroup delete duplicate to_front to_back
		align_left align_hcenter align_right align_top align_vcenter align_bottom)
	(enum size text))

(defun trans (%0)
	;transparent colour
	(+ (logand 0xffffff %0) 0x60000000))

(defq +min_width 480 +min_height 320 +paper_col 0xffF8F8FF +paper_ink 0xffADD8E6
	;what a pen draws for each button of the mode bar, and how wide for each of the widths
	*modes* (list :select :pen :line :arrow :arrow2 :rect :ellipse :frect :fellipse :text :eraser)
	*widths* (list 3.0 6.0 12.0)
	*styles* (list :plain :grid :lines :axis)
	*palette* (list +argb_black +argb_white +argb_red +argb_green +argb_blue +argb_cyan +argb_yellow +argb_magenta)
	*palette* (cat *palette* (map (const trans) *palette*))
	*board* (Board))

(ui-window *window* ()
	(ui-title-bar _ "Whiteboard" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_undo +sym_redo +sym_new +sym_open +sym_save +sym_clear +sym_cut) +event_undo))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_lines +sym_axis)
			(:color *env_toolbar2_col*)) :connect +event_style)
		(ui-tool-bar *snap_toolbar* ()
			(ui-buttons (+sym_snap +sym_snap_angle +sym_lock) +event_snap))
		(ui-tool-bar *zoom_toolbar* (:color *env_toolbar2_col*)
			(ui-buttons (+sym_zoom_in +sym_zoom_out) +event_zoom_in))
		(. (ui-textfield *size_field* (:color +argb_white :hint_text "width x height" :clear_text "1024x768"
			:min_width 120)) :connect +event_size)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *mode_toolbar* (+sym_select +sym_pen +sym_line +sym_arrow +sym_double_arrow
				+sym_rect +sym_circle +sym_filled_rect +sym_filled_circle +sym_text +sym_eraser)
			(:color *env_toolbar2_col*)) :connect +event_mode)
		(. (ui-radio-bar *radius_toolbar* (+sym_small +sym_medium +sym_large)) :connect +event_radius)
		(ui-tool-bar *tool_toolbar* (:color *env_toolbar2_col*)
			(ui-buttons (+sym_ruler +sym_protractor +sym_set_square +sym_full_protractor) +event_ruler))
		(. (ui-textfield *text_field* (:color +argb_white :hint_text "words to put down" :clear_text ""
			:min_width 160)) :connect +event_text)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *selection_toolbar* ()
			(ui-buttons (+sym_group +sym_ungroup +sym_delete +sym_duplicate +sym_to_front +sym_to_back
				+sym_align_left +sym_align_hcenter +sym_align_right
				+sym_align_top +sym_align_vcenter +sym_align_bottom) +event_group))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *ink_toolbar* (+sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen
				+sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush)
				(:font *env_medium_symbol_font* :color *env_toolbar2_col*)) :connect +event_ink))
	(ui-scroll *image_scroll* +scroll_flag_both
			(:min_width +min_width :min_height +min_height)
		(ui-flow *board_stack* (:flow_flags +flow_stack_fill)
			(ui-element *board_view* (Board-view *board*))
			;what is round the document, the desk it lies on
			(ui-backdrop *backdrop* (:color 0xffa8acb0)))))

(ui-tool-tips *main_toolbar*
	'("undo" "redo" "new" "open" "save" "clear" "clip away what is off the paper"))

(ui-tool-tips *style_toolbar*
	'("plain" "grid" "lines" "axis"))

(ui-tool-tips *snap_toolbar*
	'("snap to the grid" "snap angles" "a corner keeps the shape, or is free"))

(ui-tool-tips *zoom_toolbar*
	'("zoom in" "zoom out"))

(ui-tool-tips *mode_toolbar*
	'("select" "pen" "line" "arrow" "double arrow" "rect" "ellipse"
	"filled rect" "filled ellipse" "words" "eraser"))

(ui-tool-tips *radius_toolbar*
	'("thin" "medium" "thick"))

(ui-tool-tips *tool_toolbar*
	'("ruler" "protractor" "set square" "whole protractor"))


(ui-tool-tips *selection_toolbar*
	'("group" "ungroup" "delete" "duplicate" "to the front" "to the back"
	"align left" "align centres" "align right" "align top" "align middles" "align bottom"))

(each (# (def %0 :ink_color %1)) (. *ink_toolbar* :children) *palette*)
(. *ink_toolbar* :set_selected 0)
(. *mode_toolbar* :set_selected 1)
(. *radius_toolbar* :set_selected 0)
(. *style_toolbar* :set_selected 1)
(def *board_view* :scroll *image_scroll*)
