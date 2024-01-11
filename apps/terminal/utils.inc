;;;;;;;
; utils
;;;;;;;

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y* buffer (. *edit* :get_buffer))
		(. buffer :set_cursor x y)
		(bind '(ax ay) (. *edit* :get_anchor))
		(unless (= ay y)
			(setq ax x ay y)
			(. *edit* :set_anchor ax ay))
		~_
		(bind '(x y) (. buffer :get_cursor))
		(setq *cursor_x* x *cursor_y* y)))
