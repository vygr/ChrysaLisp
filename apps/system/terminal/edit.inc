;;;;;;;;;;;;;;;;;
; editing actions
;;;;;;;;;;;;;;;;;

(defun insert-line-wrapped (buffer line wrap_width)
	(bind '(cx &ignore) (. buffer :get_cursor))
	(when (> (+ cx (length line)) wrap_width)
		(.-> buffer
			(:insert (slice line 0 (setq cx (- wrap_width cx))))
			(:insert +char_class_lf))
		(setq line (slice line cx -1)))
	(when (> (length line) wrap_width)
		(defq lines (partition line wrap_width) line (pop lines))
		(each (# (.-> buffer (:insert %0) (:insert +char_class_lf))) lines))
	(. buffer :insert line))

(defun wrap-text (text &optional wrap_width)
	(setd wrap_width +max_int)
	(while (defq idx (find +char_class_lf text))
		(when (> idx 0) (task-slice)
			(insert-line-wrapped buffer (slice text 0 idx) wrap_width))
		(. buffer :insert +char_class_lf)
		(setq text (slice text (inc idx) -1)))
	(when (> (length text) 0) (insert-line-wrapped buffer text wrap_width)))

(defun action-insert (text &optional wrap_width)
	(input-cursor (wrap-text text wrap_width))
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
								(push history (trim cmd))
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

(defun action-abort ()
	(when *pipe*
		(input-cursor
			;abort
			(. *pipe* :abort)
			(setq *pipe* :nil)
			(. buffer :insert (cat +char_class_lf *env_terminal_prompt*)))
		(setq *margin_x* *cursor_x*)
		(refresh)))
