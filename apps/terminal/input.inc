;;;;;;;;;;;;;
; input utils
;;;;;;;;;;;;;

(defq *line_buf* "" *line_pos* 0 *line_history* (list) *line_history_idx* 0)

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
		((= c 0x40000050)
			;cursor left key ?
			(setq *line_pos* (max 0 (dec *line_pos*))))
		((= c 0x4000004f)
			;cursor right key ?
			(setq *line_pos* (min (length *line_buf*) (inc *line_pos*))))
		((= c 0x40000052)
			;cursor up key ?
			(setq *line_buf* "" *line_history_idx* (max 0 (dec *line_history_idx*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*)))
			(setq *line_pos* (length *line_buf*)))
		((= c 0x40000051)
			;cursor down key ?
			(setq *line_buf* "" *line_history_idx* (min (inc *line_history_idx*) (length *line_history*)))
			(when (< *line_history_idx* (length *line_history*))
				(setq *line_buf* (elem *line_history_idx* *line_history*)))
			(setq *line_pos* (length *line_buf*)))
		((and (= c 8) (/= *line_pos* 0))
			;backspace key ?
			(setq *line_buf* (erase *line_buf* (setq *line_pos* (dec *line_pos*)) (inc *line_pos*))))
		((<= 32 c 127)
			;insert the char
			(setq *line_buf* (insert *line_buf* *line_pos* (char c)) *line_pos* (inc *line_pos*)))))

(defun-bind line-clear ()
	;clear line buffer
	(setq *line_buf* "" *line_pos* 0))

(defun-bind line-with-cursor ()
	;line buffer with cursor
	(if (= (length *line_buf*) *line_pos*)
		(cat *line_buf* (const (ascii-char (+ 32 128))))
		(cat (slice 0 *line_pos* *line_buf*)
			(char (logior 0x80 (code (elem *line_pos* *line_buf*))))
			(slice (inc *line_pos*) -1 *line_buf*))))
