;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defun macro-playback ()
	(some! 0 -1 :t (# (defq func (elem-get 0 %0))
			(cond
				((or (eql func (const action-find-up))
						(eql func (const action-find-down)))
					(setq running (eval %0)))
				(:t (eval %0) :t)))
		(list *macro_actions*)))

(defun action-macro-record ()
	(if (setq *macro_record* (not *macro_record*))
		(clear *macro_actions*))
	(radio-select macro_toolbar (list :nil :nil :nil *macro_record*))
	(refresh))

(defun action-macro-playback ()
	(when (> (length *macro_actions*) 0)
		(defq buffer (. *edit* :get_buffer) running :t)
		(push *refresh_mode* 1)
		(undoable (macro-playback))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-to-eof ()
	(when (> (length *macro_actions*) 0)
		(defq buffer (. *edit* :get_buffer) running :t)
		(push *refresh_mode* 1)
		(undoable
			(bind '(cx cy) (. buffer :get_cursor))
			(bind '(eof_gap_x eof_gap_y) (eof-gaps))
			(defq old_eof_gap_x +max_int  old_eof_gap_y +max_int)
			(while (and running (> eof_gap_y 0)
						(or (< eof_gap_y old_eof_gap_y)
							(and (= eof_gap_y old_eof_gap_y) (< eof_gap_x old_eof_gap_x)))
						(not (some! cy -1 :t (const blank-line?)
							(list (. buffer :get_text_lines)))))
				(macro-playback)
				(when running
					(setq old_eof_gap_x eof_gap_x old_eof_gap_y eof_gap_y)
					(bind '(cx cy) (. buffer :get_cursor))
					(bind '(eof_gap_x eof_gap_y) (eof-gaps)))))
		(pop *refresh_mode*)
		(refresh)))

(defun action-macro-global ()
	(for-all-buffers
		(.-> *edit* (:set_cursor 0 0) (:set_anchor 0 0) (:set_find 0 0 0 0))
		(action-macro-to-eof)))
