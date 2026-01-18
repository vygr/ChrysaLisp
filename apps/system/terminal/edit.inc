;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun action-insert (string &optional wrap_len)
	(input-cursor (. buffer :insert string))
	(bind '(w h) (. buffer :get_size))
	(when (> h *env_terminal_lines*)
		(.-> buffer (:set_cursor 0 0 0 (- h *env_terminal_lines*)) :delete)
		(setq *cursor_y* (- *cursor_y* (- h *env_terminal_lines*)))
		(. buffer :set_cursor *cursor_x* *cursor_y*))
	(refresh))

(defun action-backspace ()
	(input-cursor (if (or (> x *margin_x*) (/= x ax)) (. buffer :backspace)))
	(refresh))

(defun action-delete ()
	(input-cursor (. buffer :delete))
	(refresh))

(defun action-tab ()
	(unless *pipe*
		(input-cursor
			(defq cmd (slice (. buffer :get_text_line y) *margin_x* -2))
			(. buffer :insert (url-ext cmd (- x *margin_x*))))
		(refresh)))

(defun action-break ()
	(input-cursor
		(bind '(x y &ignore) (. *edit* :clip_cursor +max_int y))
		(. buffer :set_cursor x y)
		(cond
			(*pipe*
				;active pipe so feed it
				(. *pipe* :write (cat (slice (. buffer :get_text_line y) *margin_x* -2) +char_class_lf))
				(. buffer :insert +char_class_lf))
			(:t ;no active pipe
				(defq cmd (slice (. buffer :get_text_line y) *margin_x* -2))
				(. buffer :insert (const (cat +char_class_lf +char_class_lf)))
				(cond
					((nql cmd "")
						;start pipe
						(catch (setq *pipe* (Pipe cmd *select*)) (progn (setq *pipe* :nil) :t))
						(if *pipe*
							;push new line history entry if not same as last entry
							(unless (eql cmd "")
								(bind '(history) (gather *meta_map* :history))
								(if (defq i (find cmd history))
									(setq history (erase history i (inc i))))
								(push history cmd)
								(scatter *meta_map* :history history)
								(setq *history_idx* (length history)))
							;error with pipe
							(. buffer :insert (cat "Pipe Error !"
								+char_class_lf +char_class_lf
								*env_terminal_prompt*))))
					(:t (. buffer :insert *env_terminal_prompt*))))))
	(refresh))

(defun action-escape ()
	(when *pipe*
		(input-cursor
			;EOF
			(. *pipe* :close)
			(setq *pipe* :nil)
			(. buffer :insert (cat +char_class_lf *env_terminal_prompt*)))
		(setq *margin_x* *cursor_x*)
		(refresh)))
