(enums +event 0
	(enum close max min)
	(enum save load clear undo redo)
	(enum grid plain axis lines)
	(enum radius1 radius2 radius3)
	(enum pen line arrow1 arrow2 box circle fbox fcircle)
	(enum black white red green blue cyan yellow magenta
		tblack twhite tred tgreen tblue tcyan tyellow tmagenta))

(defun trans (_)
	;transparent colour
	(+ (logand 0xffffff _) 0x60000000))

(defq +canvas_width 1024 +canvas_height 768 +min_width 320 +min_height 240
	*palette* (list +argb_black +argb_white +argb_red +argb_green +argb_blue +argb_cyan +argb_yellow +argb_magenta)
	*palette* (cat *palette* (map trans *palette*)))

(ui-window *window* ()
	(ui-title-bar _ "Whiteboard" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xea07 0xe9e9 0xe970 0xe9fe 0xe99d) +event_save))
		(ui-tool-bar *style_toolbar* ()
			(ui-buttons (0xe976 0xe9a3 0xe9d4 0xe9f0) +event_grid))
		(ui-tool-bar *radius_toolbar* ()
			(ui-buttons (0xe979 0xe97d 0xe97b) +event_radius1))
		(ui-tool-bar *mode_toolbar* ()
			(ui-buttons (0xe9ec 0xe9d8 0xe917 0xea20 0xe9f6 0xe94b 0xe960 0xe95f) +event_pen)))
	(ui-tool-bar *ink_toolbar* (:font *env_medium_toolbar_font* :color (const *env_toolbar2_col*))
		(each (lambda (col)
			(. (ui-button __ (:ink_color col :text
				(if (< _ 8) (const (num-to-utf8 0xe982)) (const (num-to-utf8 0xea04))))) :connect
					(+ _ +event_black))) *palette*))
	(ui-scroll *image_scroll* +scroll_flag_both
			(:min_width +min_width :min_height +min_height)
		(ui-backdrop *backdrop* (:color 0xffF8F8FF :ink_color 0xffADD8E6 :style :grid)
			(ui-canvas *overlay_canvas* +canvas_width +canvas_height 1)
			(ui-canvas *commited_canvas* +canvas_width +canvas_height 1))))

(ui-tool-tips *main_toolbar*
	'("save" "open" "clear" "undo" "redo"))

(ui-tool-tips *style_toolbar*
	'("plain" "grid" "lines" "axis"))

(ui-tool-tips *radius_toolbar*
	'("small" "medium" "large"))

(ui-tool-tips *mode_toolbar*
	'("pen" "line" "arrow" "double arrow" "rect"
	"circle" "filled rect" "filled circle"))

(. *backdrop* :set_size +canvas_width +canvas_height)
(. *image_scroll* :layout)
