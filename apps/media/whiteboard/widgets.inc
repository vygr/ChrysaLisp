(enums +event +event_user
	(enum undo redo save load clear)
	(enum stroke)
	(enum style radius ink)
	(enum select move pen line arrow1 arrow2 box circle fbox fcircle)
	(enum group ungroup delete align_left align_vcenter align_right align_top align_hcenter align_bottom))

(bits +group 0
	(bit selected))

(defun trans (%0)
	;transparent colour
	(+ (logand 0xffffff %0) 0x60000000))

(defq +canvas_width 1024 +canvas_height 768 +min_width 320 +min_height 240
	*palette* (list +argb_black +argb_white +argb_red +argb_green +argb_blue +argb_cyan +argb_yellow +argb_magenta)
	*palette* (cat *palette* (map (const trans) *palette*)))

(ui-window *window* ()
	(ui-title-bar _ "Whiteboard" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (+sym_undo +sym_redo +sym_save +sym_open +sym_clear) +event_undo))
		(. (ui-radio-bar *style_toolbar* (+sym_plain +sym_grid +sym_lines +sym_axis)
			(:color *env_toolbar2_col*)) :connect +event_style)
		(. (ui-radio-bar *radius_toolbar* (+sym_small +sym_medium +sym_large)) :connect +event_radius)
		(. (ui-radio-bar *mode_toolbar* (+sym_select +sym_move +sym_pen +sym_line +sym_arrow +sym_double_arrow +sym_rect +sym_circle +sym_filled_rect +sym_filled_circle)
			(:color *env_toolbar2_col*)) :connect +event_select)
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *selection_toolbar* ()
			(ui-buttons (+sym_group +sym_ungroup +sym_delete +sym_align_left +sym_align_vcenter +sym_align_right +sym_align_top +sym_align_hcenter +sym_align_bottom) +event_group))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-radio-bar *ink_toolbar* (+sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen +sym_pen
				+sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush +sym_brush)
				(:font *env_medium_symbol_font* :color *env_toolbar2_col*)) :connect +event_ink))
	(ui-scroll *image_scroll* +scroll_flag_both
			(:min_width +min_width :min_height +min_height)
		(ui-flow *strokes_stack* (:flow_flags +flow_stack_fill)
			(. (ui-stroke *strokes*) :connect +event_stroke)
			(ui-backdrop *backdrop* (:color 0xffF8F8FF :ink_color 0xffADD8E6 :style :grid)
				(ui-canvas *committed_canvas* +canvas_width +canvas_height 1)
				(ui-canvas *staging_canvas* +canvas_width +canvas_height 1)))))

(ui-tool-tips *main_toolbar*
	'("undo" "redo" "save" "open" "clear"))

(ui-tool-tips *style_toolbar*
	'("plain" "grid" "lines" "axis"))

(ui-tool-tips *radius_toolbar*
	'("small" "medium" "large"))

(ui-tool-tips *mode_toolbar*
	'("select" "move" "pen" "line" "arrow" "double arrow" "rect"
	"circle" "filled rect" "filled circle"))

(ui-tool-tips *selection_toolbar*
	'("group" "ungroup" "delete" "align left" "align vcenter" "align right" "align top" "align hcenter" "align bottom"))

(each (# (def %0 :ink_color %1)) (. *ink_toolbar* :children) *palette*)
(. *ink_toolbar* :set_selected 0)
(. *mode_toolbar* :set_selected 2)
(. *radius_toolbar* :set_selected 0)
(. *style_toolbar* :set_selected 0)
(. *strokes_stack* :change 0 0 +canvas_width +canvas_height)
(. *image_scroll* :layout)
