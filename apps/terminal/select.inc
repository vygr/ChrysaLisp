;;;;;;;;;;;;;;;;
; select actions
;;;;;;;;;;;;;;;;

(defun action-select-word ()
	(bind '(x x1 y) (select-word))
	(setq *anchor_x* x *anchor_y* y)
	(. *current_buffer* :set_cursor x1 y)
	(refresh))

(defun action-select-line ()
	(bind '(y y1) (select-line))
	(setq *anchor_x* 0 *anchor_y* y)
	(. *current_buffer* :set_cursor 0 y1)
	(refresh))

(defun action-select-paragraph ()
	(bind '(y y1) (select-paragraph))
	(setq *anchor_x* 0 *anchor_y* y)
	(. *current_buffer* :set_cursor 0 y1)
	(refresh))
