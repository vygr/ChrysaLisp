;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(input-cursor
		(cond
			(*pipe* (. buffer :set_cursor 0 y))
			((. buffer :set_cursor (length *env_terminal_prompt*) y))))
	(clear-selection) (refresh))

(defun action-end ()
	(input-cursor
		(bind '(x y) (. buffer :constrain +max_int y))
		(. buffer :set_cursor x y))
	(clear-selection) (refresh))

(defun action-left ()
	(input-cursor
		(cond
			(*pipe* (if (> x 0) (. buffer :left)))
			((if (> x (length *env_terminal_prompt*)) (. buffer :left)))))
	(clear-selection) (refresh))

(defun action-right ()
	(input-cursor
		(if (/= x (length (. buffer :get_text_line y))) (. buffer :right)))
	(clear-selection) (refresh))

(defun action-up ()
	(unless *pipe*
		(input-cursor
			(.-> buffer
				(:set_cursor 0 y)
				(:delete (length (. buffer :get_text_line y))))
			(defq cmd "")
			(setq *history_idx* (max 0 (dec *history_idx*)))
			(when (< *history_idx* (length *history*))
				(setq cmd (elem *history_idx* *history*)))
			(. buffer :insert (cat *env_terminal_prompt* cmd)))
		(clear-selection) (refresh)))

(defun action-down ()
	(unless *pipe*
		(input-cursor
			(.-> buffer
				(:set_cursor 0 y)
				(:delete (length (. buffer :get_text_line y))))
			(defq cmd "")
			(setq *history_idx* (min (inc *history_idx*) (length *history*)))
			(when (< *history_idx* (length *history*))
				(setq cmd (elem *history_idx* *history*)))
			(. buffer :insert (cat *env_terminal_prompt* cmd)))
		(clear-selection) (refresh)))
