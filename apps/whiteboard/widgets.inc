(enums +event 0
	(enum close max min)
	(enum undo redo save load clear)
	(enum stroke)
	(enum style radius ink)
	(enum pen line arrow1 arrow2 box circle fbox fcircle))

(defun trans (_)
	;transparent colour
	(+ (logand 0xffffff _) 0x60000000))

(defq +canvas_width 1024 +canvas_height 768 +min_width 320 +min_height 240
	*palette* (list +argb_black +argb_white +argb_red +argb_green +argb_blue +argb_cyan +argb_yellow +argb_magenta)
	*palette* (cat *palette* (map (const trans) *palette*)))

(ui-window *window* ()
	(ui-title-bar _ "Whiteboard" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9fe 0xe99d 0xea07 0xe9e9 0xe970) +event_undo))
		(. (ui-radio-bar *style_toolbar* (0xe976 0xe9a3 0xe9d4 0xe9f0)
			(:color *env_toolbar2_col*)) :connect +event_style)
		(. (ui-radio-bar *radius_toolbar* (0xe979 0xe97d 0xe97b)) :connect +event_radius)
		(. (ui-radio-bar *mode_toolbar* (0xe9ec 0xe9d8 0xe917 0xea20 0xe9f6 0xe94b 0xe960 0xe95f)
			(:color *env_toolbar2_col*)) :connect +event_pen))
	(. (ui-radio-bar *ink_toolbar* (0xe982 0xe982 0xe982 0xe982 0xe982 0xe982 0xe982 0xe982
			0xea04 0xea04 0xea04 0xea04 0xea04 0xea04 0xea04 0xea04)
			(:font *env_medium_toolbar_font* :color *env_toolbar2_col*))
		:connect +event_ink)
	(ui-scroll *image_scroll* +scroll_flag_both
			(:min_width +min_width :min_height +min_height)
		(ui-flow *strokes_stack* (:flow_flags +flow_stack_fill)
			(. (ui-stroke *strokes*) :connect +event_stroke)
			(ui-backdrop *backdrop* (:color 0xffF8F8FF :ink_color 0xffADD8E6 :style :grid)
				(ui-canvas *overlay_canvas* +canvas_width +canvas_height 1)
				(ui-canvas *commited_canvas* +canvas_width +canvas_height 1)))))

(ui-tool-tips *main_toolbar*
	'("undo" "redo" "save" "open" "clear"))

(ui-tool-tips *style_toolbar*
	'("plain" "grid" "lines" "axis"))

(ui-tool-tips *radius_toolbar*
	'("small" "medium" "large"))

(ui-tool-tips *mode_toolbar*
	'("pen" "line" "arrow" "double arrow" "rect"
	"circle" "filled rect" "filled circle"))

(each (# (def %0 :ink_color %1)) (. *ink_toolbar* :children) *palette*)
(. *ink_toolbar* :set_selected 0)
(. *mode_toolbar* :set_selected 0)
(. *radius_toolbar* :set_selected 0)
(. *style_toolbar* :set_selected 1)
(. *strokes_stack* :change 0 0 +canvas_width +canvas_height)
(. *image_scroll* :layout)
