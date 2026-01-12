;;;;;;;
; utils
;;;;;;;

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y* buffer (. *edit* :get_buffer))
		(bind '(_ _ ax ay) (. *edit* :get_cursor))
		(unless (= ay y) (setq ax x ay y))
		(. buffer :set_cursor x y ax ay)
		~_
		(bind '(x y &ignore) (. buffer :get_cursor))
		(setq *cursor_x* x *cursor_y* y)))
