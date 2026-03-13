;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/chess/widgets.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(enums +event 0
	(enum close)
	(enum reset undo redo)
	(enum button))

(defq vdu_width 38 vdu_height 12 text_buf :nil)

(ui-window *window* (:color +argb_black)
	(ui-title-bar _ "Chess" (0xea19) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar *toolbar* ()
			(ui-buttons (0xe972 0xe9fe 0xe99d) +event_reset))
		(ui-backdrop _ (:color (const *env_toolbar_col*))))
	(ui-grid chess_grid (:grid_width 8 :grid_height 8 :font (create-font "fonts/Chess.ctf" 42)
			:border 1 :text " ")
		(each (lambda (i)
			(. (ui-button _ (:color 0 :ink_color 0)) :connect (+ +event_button i))) (range 0 64)))
	(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color +argb_cyan)))

(ui-tool-tips *toolbar* '("reset game" "undo move" "redo move"))