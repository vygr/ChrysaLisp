;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(input-cursor
		(cond
			(*pipe* (. buffer :set_cursor 0 y))
			((. buffer :set_cursor (length *env_terminal_prompt*) y))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-home-select ()
	(input-cursor
		(cond
			(*pipe* (. buffer :set_cursor 0 y))
			((. buffer :set_cursor (length *env_terminal_prompt*) y))))
	(refresh))

(defun action-end ()
	(input-cursor
		(bind '(x y) (. buffer :constrain +max_int y))
		(. buffer :set_cursor x y))
	(. *edit* :clear_selection)
	(refresh))

(defun action-end-select ()
	(input-cursor
		(bind '(x y) (. buffer :constrain +max_int y))
		(. buffer :set_cursor x y))
	(refresh))

(defun action-left ()
	(input-cursor
		(cond
			(*pipe* (if (> x 0) (. buffer :left)))
			((< ax x) (. buffer :set_cursor ax y))
			((if (> x (length *env_terminal_prompt*)) (. buffer :left)))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-left-select ()
	(input-cursor
		(cond
			(*pipe* (if (> x 0) (. buffer :left)))
			((if (> x (length *env_terminal_prompt*)) (. buffer :left)))))
	(refresh))

(defun action-right ()
	(input-cursor
		(cond
			((> ax x) (. buffer :set_cursor ax y))
			((if (/= x (length (. buffer :get_text_line y))) (. buffer :right)))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-right-select ()
	(input-cursor
		(cond
			(*pipe* (. buffer :right))
			((if (/= x (length (. buffer :get_text_line y))) (. buffer :right)))))
	(refresh))

(defun action-up ()
	(unless *pipe*
		(input-cursor
			(bind '(history) (gather *meta_map* :history))
			(.-> buffer
				(:set_cursor 0 y)
				(:delete (length (. buffer :get_text_line y))))
			(defq cmd "")
			(setq *history_idx* (max 0 (dec *history_idx*)))
			(when (< *history_idx* (length history))
				(setq cmd (elem-get *history_idx* history)))
			(. buffer :insert (cat *env_terminal_prompt* cmd)))
		(. *edit* :clear_selection)
		(refresh)))

(defun action-down ()
	(unless *pipe*
		(input-cursor
			(bind '(history) (gather *meta_map* :history))
			(.-> buffer
				(:set_cursor 0 y)
				(:delete (length (. buffer :get_text_line y))))
			(defq cmd "")
			(setq *history_idx* (min (inc *history_idx*) (length history)))
			(when (< *history_idx* (length history))
				(setq cmd (elem-get *history_idx* history)))
			(. buffer :insert (cat *env_terminal_prompt* cmd)))
		(. *edit* :clear_selection)
		(refresh)))
