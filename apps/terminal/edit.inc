;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string)
	(input-cursor
		(. buffer :paste string))
	(bind '(w h) (. buffer :get_size))
	(when (> h *env_terminal_lines*)
		(.-> buffer (:set_cursor 0 0) (:cut 0 (- h *env_terminal_lines*)))
		(setq *cursor_y* (- *cursor_y* (- h *env_terminal_lines*)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(. buffer :clear_undo)
	(. *edit* :clear_selection)
	(refresh))

(defun action-backspace ()
	(input-cursor
		(cond
			(*pipe* (if (> x 0) (. buffer :backspace)))
			((if (> x (length *env_terminal_prompt*)) (. buffer :backspace)))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-delete ()
	(input-cursor
		(. buffer :delete))
	(. *edit* :clear_selection)
	(refresh))

(defun action-tab ()
	(unless *pipe*
		(input-cursor
			(defq prompt_len (length *env_terminal_prompt*)
				cmd (slice prompt_len -1 (. buffer :get_text_line y)))
			(. buffer :insert (url-ext cmd (- x prompt_len))))
		(. *edit* :clear_selection)
		(refresh)))

(defun action-break ()
	(input-cursor
		(bind '(x y) (. buffer :constrain +max_int y))
		(. buffer :set_cursor x y)
		(cond
			(*pipe*
				;active pipe so feed it
				(. *pipe* :write (cat (. buffer :get_text_line y) (ascii-char +char_lf)))
				(. buffer :paste (ascii-char +char_lf)))
			(t  ;no active pipe
				(defq prompt_len (length *env_terminal_prompt*)
					cmd (slice prompt_len -1 (. buffer :get_text_line y)))
				;push new line history entry if not same as last entry
				(unless (eql cmd "")
					(and (> (length (push *history* cmd)) 1)
						(eql (elem -3 *history*) cmd)
						(pop *history*))
					(setq *history_idx* (length *history*)))
				(. buffer :paste (const (cat (ascii-char +char_lf) (ascii-char +char_lf))))
				(cond
					((> (length cmd) 0)
						;start pipe
						(catch (setq *pipe* (Pipe cmd *select*)) (progn (setq *pipe* nil) t))
						(unless *pipe*
							(. buffer :paste (cat "Pipe Error !"
								(ascii-char +char_lf) (ascii-char +char_lf)
								*env_terminal_prompt*))))
					(t (. buffer :paste *env_terminal_prompt*))))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-escape ()
	(when *pipe*
		(input-cursor
			;EOF
			(. *pipe* :close)
			(setq *pipe* nil)
			(. buffer :paste (cat (ascii-char +char_lf) *env_terminal_prompt*)))
		(. *edit* :clear_selection)
		(refresh)))
