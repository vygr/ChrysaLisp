;;;;;;;;;;;;;
; input utils
;;;;;;;;;;;;;

(defq *line_buf* "" *line_history* (list) *line_history_idx* 0)

(defun-bind line-input (c)
	;key action on line input buffer
	(cond
		((or (= c 10) (= c 13))
			;enter key ?
			(when (/= (length *line_buf*) 0)
				;push new line history entry if not same as last entry
				(and (> (length (push *line_history* *line_buf*)) 1)
					(eql (elem -3 *line_history*) *line_buf*)
					(pop *line_history*))
				(setq *line_history_idx* (length *line_history*))))
		((= c 27))
			;esc key
		((= c 0x40000052)
			;cursor up key ?
			(setq *line_buf* "" *line_history_idx* (max 0 (dec *line_history_idx*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*))))
		((= c 0x40000051)
			;cursor down key ?
			(setq *line_buf* "" *line_history_idx* (min (inc *line_history_idx*) (length *line_history*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*))))
		((and (= c 8) (/= (length *line_buf*) 0))
			;backspace key ?
			(setq *line_buf* (slice 0 -2 *line_buf*)))
		((<= 32 c 127)
			;append the char
			(setq *line_buf* (cat *line_buf* (char c))))))
