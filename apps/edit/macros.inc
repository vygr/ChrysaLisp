;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defun action-macro-record ()
	(setq *macro_record* (not *macro_record*))
	(cond
		(*macro_record*
			(radio-select macro_toolbar 2)
			(clear *macro_actions*))
		(t  (radio-select macro_toolbar -1)))
	(. *edit* :clear_selection)
	(refresh))

(defun action-macro-playback ()
	(when (> (length *macro_actions*) 0)
		(let ((:macro_playback t))
			(undoable nil
				(each (# (eval %0)) *macro_actions*)))
		(. *edit* :clear_selection)
		(refresh)))

(defun action-macro-to-eof ()
	(when (> (length *macro_actions*) 0)
		(let ((:macro_playback t))
			(undoable nil
				(bind '(cx cy) (. buffer :get_cursor))
				(defq eof_gap_x (- (length (. buffer :get_text_line cy)) cx)
					eof_gap_y (- (elem 1 (. buffer :get_size)) cy)
					old_eof_gap_x +max_int  old_eof_gap_y +max_int)
				(while (and (> eof_gap_y 0)
							(or (< eof_gap_y old_eof_gap_y)
								(and (= eof_gap_y old_eof_gap_y) (< eof_gap_x old_eof_gap_x)))
							(not (some! cy -1 t (const blank-line?)
								(list (. buffer :get_text_lines)))))
					(each (# (eval %0)) *macro_actions*)
					(bind '(cx cy) (. buffer :get_cursor))
					(setq old_eof_gap_x eof_gap_x old_eof_gap_y eof_gap_y
						eof_gap_x (- (length (. buffer :get_text_line cy)) cx)
						eof_gap_y (- (elem 1 (. buffer :get_size)) cy)))))
		(. *edit* :clear_selection)
		(refresh)))
