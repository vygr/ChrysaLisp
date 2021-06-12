;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(set-input-cursor)
	(bind '(w h) (.-> *current_buffer* (:paste string) :get_size))
	(when (> h *env_terminal_lines*)
		(.-> *current_buffer*
			(:set_cursor 0 0)
			(:cut 0 (- h *env_terminal_lines*)))
		(bind '(w h) (. *current_buffer* :get_size))
		(. *current_buffer* :set_cursor 0 (dec h)))
	(bind '(x y) (. *current_buffer* :get_cursor))
	(setq *cursor_x* x *cursor_y* y)
	(. *current_buffer* :clear_undo)
	(clear-selection) (refresh))

(defun action-backspace ()
	(bind '(x y) (set-input-cursor))
	(cond
		(*pipe* (if (> x 0) (. *current_buffer* :backspace)))
		((if (> x (length *env_terminal_prompt*)) (. *current_buffer* :backspace))))
	(clear-selection) (refresh))

(defun action-delete ()
	(set-input-cursor)
	(. *current_buffer* :delete)
	(clear-selection) (refresh))

(defun action-tab ()
	(unless *pipe*
		(bind '(x y) (set-input-cursor))
		(defq prompt_len (length *env_terminal_prompt*)
			cmd (slice prompt_len -1 (. *current_buffer* :get_text_line y)))
		(. *current_buffer* :insert (url-ext cmd (- x prompt_len)))
		(clear-selection) (refresh)))

(defun action-break ()
	(bind '(x y) (set-input-cursor))
	(cond
		(*pipe*
			;active pipe so feed it
			(. *pipe* :write (cat (. *current_buffer* :get_text_line y) (ascii-char +char_lf)))
			(action-insert (ascii-char +char_lf)))
		(t  ;no active pipe
			(defq prompt_len (length *env_terminal_prompt*)
				cmd (slice prompt_len -1 (. *current_buffer* :get_text_line y)))
			;push new line history entry if not same as last entry
			(unless (eql cmd "")
				(and (> (length (push *line_history* cmd)) 1)
					(eql (elem -3 *line_history*) cmd)
					(pop *line_history*))
				(setq *line_history_idx* (length *line_history*)))
			(action-insert (ascii-char +char_lf))
			(cond
				((> (length cmd) 0)
					;start pipe
					(catch (setq *pipe* (Pipe cmd *select*)) (progn (setq *pipe* nil) t))
					(unless *pipe*
						(action-insert (cat "Pipe Error !" (ascii-char +char_lf) *env_terminal_prompt*))))
				(t (action-insert *env_terminal_prompt*))))))

(defun action-escape ()
	(when *pipe*
		;feed active pipe, then EOF
		(bind '(x y) (set-input-cursor))
		(defq cmd (. *current_buffer* :get_text_line y))
		(unless (eql cmd "") (. *pipe* :write cmd))
		(. *pipe* :close) (setq *pipe* nil)
		(action-insert (cat (ascii-char +char_lf) *env_terminal_prompt*))))
