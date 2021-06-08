;;;;;;;;;;;;;;;;
; select actions
;;;;;;;;;;;;;;;;

(defun action-select-all ()
	(bind '(w h) (. *current_buffer* :get_size))
	(setq *anchor_x* 0 *anchor_y* 0 *shift_select* t)
	(. *current_buffer* :set_cursor 0 h)
	(refresh))

(defun action-select-word ()
	(bind '(x x1) (select-word))
	(setq *anchor_x* x *anchor_y* *cursor_y* *shift_select* t)
	(. *current_buffer* :set_cursor x1 *cursor_y*)
	(refresh))

(defun action-select-line ()
	(setq *anchor_x* 0 *anchor_y* *cursor_y* *shift_select* t)
	(. *current_buffer* :set_cursor 0 (inc *cursor_y*))
	(refresh))

(defun action-select-paragraph ()
	(bind '(y y1) (select-paragraph))
	(setq *anchor_x* 0 *anchor_y* y *shift_select* t)
	(. *current_buffer* :set_cursor 0 y1)
	(refresh))

(defun action-select-block ()
	(when (bind '(x y) (. (defq buffer *current_buffer*) :left_bracket))
		(when (bind '(x1 y1) (. buffer :right_bracket))
			(setq *anchor_x* x *anchor_y* y *shift_select* t)
			(. buffer :set_cursor (inc x1) y1)
			(refresh))))
