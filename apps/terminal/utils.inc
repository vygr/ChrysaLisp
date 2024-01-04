;;;;;;;
; utils
;;;;;;;

(defmacro input-cursor (&rest _)
	`(progn
		(defq x *cursor_x* y *cursor_y* buffer (. *edit* :get_buffer))
		(. buffer :set_cursor x y)
		~_
		(bind '(x y) (. buffer :get_cursor))
		(setq *cursor_x* x *cursor_y* y)))
