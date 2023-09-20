;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defun action-macro-record ()
	(if (setq *macro_record* (not *macro_record*))
		(clear *macro_actions*))
	(radio-select macro_toolbar (list :nil :nil :nil *macro_record*))
	(refresh))

(defun action-macro-playback ()
	(when (> (length *macro_actions*) 0)
		(push *refresh_mode* 1)
		(defq buffer (. *edit* :get_buffer))
		(undoable (each (# (eval %0)) *macro_actions*))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-to-eof ()
	(when (> (length *macro_actions*) 0)
		(defq buffer (. *edit* :get_buffer))
		(push *refresh_mode* 1)
		(undoable
			(bind '(cx cy) (. buffer :get_cursor))
			(defq eof_gap_x (- (length (. buffer :get_text_line cy)) cx)
				eof_gap_y (- (elem-get 1 (. buffer :get_size)) cy)
				old_eof_gap_x +max_int  old_eof_gap_y +max_int)
			(while (and (> eof_gap_y 0)
						(or (< eof_gap_y old_eof_gap_y)
							(and (= eof_gap_y old_eof_gap_y) (< eof_gap_x old_eof_gap_x)))
						(not (some! cy -1 :t (const blank-line?)
							(list (. buffer :get_text_lines)))))
				(each (# (eval %0)) *macro_actions*)
				(bind '(cx cy) (. buffer :get_cursor))
				(setq old_eof_gap_x eof_gap_x old_eof_gap_y eof_gap_y
					eof_gap_x (- (length (. buffer :get_text_line cy)) cx)
					eof_gap_y (- (elem-get 1 (. buffer :get_size)) cy))))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-global ()
	(when (defq exit :nil here *current_file*)
		(push *refresh_mode* 1)
		(until exit
			(action-macro-to-eof)
			(update-meta-data)
			(action-next-buffer)
			(setq exit (eql *current_file* here)))
		(pop *refresh_mode*)
		(refresh)))
