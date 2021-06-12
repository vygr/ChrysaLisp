;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(bind '(x y) (set-input-cursor))
	(cond
		(*pipe* (. *current_buffer* :set_cursor 0 y))
		((. *current_buffer* :set_cursor (length *env_terminal_prompt*) y)))
	(clear-selection) (refresh))

(defun action-end ()
	(bind '(x y) (set-input-cursor))
	(bind '(x y) (. *current_buffer* :constrain +max_int y))
	(. *current_buffer* :set_cursor x y)
	(clear-selection) (refresh))

(defun action-left ()
	(bind '(x y) (set-input-cursor))
	(cond
		(*pipe* (if (> x 0) (. *current_buffer* :left)))
		((if (> x (length *env_terminal_prompt*)) (. *current_buffer* :left))))
	(clear-selection) (refresh))

(defun action-right ()
	(bind '(x y) (set-input-cursor))
	(if (/= x (length (. *current_buffer* :get_text_line y))) (. *current_buffer* :right))
	(clear-selection) (refresh))

(defun action-up ()
	(unless *pipe*
		(bind '(x y) (set-input-cursor))
		(.-> *current_buffer*
			(:set_cursor 0 y)
			(:delete (length (. *current_buffer* :get_text_line y))))
		(defq cmd "")
		(setq *line_history_idx* (max 0 (dec *line_history_idx*)))
		(when (< *line_history_idx* (length *line_history*))
			(setq cmd (elem *line_history_idx* *line_history*)))
		(. *current_buffer* :insert (cat *env_terminal_prompt* cmd))
		(clear-selection) (refresh)))

(defun action-down ()
	(unless *pipe*
		(bind '(x y) (set-input-cursor))
		(.-> *current_buffer*
			(:set_cursor 0 y)
			(:delete (length (. *current_buffer* :get_text_line y))))
		(defq cmd "")
		(setq *line_history_idx* (min (inc *line_history_idx*) (length *line_history*)))
		(when (< *line_history_idx* (length *line_history*))
			(setq cmd (elem *line_history_idx* *line_history*)))
		(. *current_buffer* :insert (cat *env_terminal_prompt* cmd))
		(clear-selection) (refresh)))
