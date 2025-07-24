;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string &optional wrap_len)
	(input-cursor
		(. buffer :cut ax ay)
		(. buffer :paste string wrap_len))
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
			(*pipe* (if (> x *margin_x*) (. buffer :backspace)))
			((not (eql (. buffer :cut ax ay) "")))
			((if (> x *margin_x*) (. buffer :backspace)))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-delete ()
	(input-cursor
		(cond
			((not (eql (. buffer :cut ax ay) "")))
			((. buffer :delete))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-tab ()
	(unless *pipe*
		(input-cursor
			(defq cmd (slice (. buffer :get_text_line y) *margin_x* -1))
			(. buffer :insert (url-ext cmd (- x *margin_x*))))
		(. *edit* :clear_selection)
		(refresh)))

(defun action-break ()
	(input-cursor
		(bind '(x y) (. buffer :constrain +max_int y))
		(. buffer :set_cursor x y)
		(cond
			(*pipe*
				;active pipe so feed it
				(. *pipe* :write (cat (slice (. buffer :get_text_line y) *margin_x* -1) (ascii-char +char_lf)))
				(. buffer :paste (ascii-char +char_lf)))
			(:t ;no active pipe
				(defq cmd (slice (. buffer :get_text_line y) *margin_x* -1))
				(. buffer :paste (const (cat (ascii-char +char_lf) (ascii-char +char_lf))))
				(cond
					((> (length cmd) 0)
						;start pipe
						(catch (setq *pipe* (Pipe cmd *select*)) (progn (setq *pipe* :nil) :t))
						(if *pipe*
							;push new line history entry if not same as last entry
							(unless (eql cmd "")
								(bind '(history) (gather *meta_map* :history))
								(and (> (length (push history cmd)) 1)
									(eql (elem-get history -3) cmd)
									(pop history))
								(setq *history_idx* (length history)))
							;error with pipe
							(. buffer :paste (cat "Pipe Error !"
								(ascii-char +char_lf) (ascii-char +char_lf)
								*env_terminal_prompt*))))
					(:t (. buffer :paste *env_terminal_prompt*))))))
	(. *edit* :clear_selection)
	(refresh))

(defun action-escape ()
	(when *pipe*
		(input-cursor
			;EOF
			(. *pipe* :close)
			(setq *pipe* :nil)
			(. buffer :paste (cat (ascii-char +char_lf) *env_terminal_prompt*)))
		(setq *margin_x* *cursor_x*)
		(. *edit* :clear_selection)
		(refresh)))
