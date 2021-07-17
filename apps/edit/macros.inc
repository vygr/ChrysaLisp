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
	(clear-selection) (refresh))

(defun action-macro-playback ()
	(when (> (length *macro_actions*) 0)
		(let ((:macro_playback t))
			(undoable
				(each (lambda (action)
						(eval action)
						(bind '(x y) (. buffer :get_cursor))
						(setq *cursor_x* x *cursor_y* y))
					*macro_actions*)))
		(clear-selection) (refresh)))

(defun action-macro-to-eof ()
	(when (> (length *macro_actions*) 0)
		(let ((:macro_playback t))
			(undoable
				(defq eof_gap_x (- (length (. buffer :get_text_line *cursor_y*)) *cursor_x*)
					eof_gap_y (- (elem 1 (. buffer :get_size)) *cursor_y*)
					old_eof_gap_x +max_int  old_eof_gap_y +max_int)
				(while (and (> eof_gap_y 0)
							(or (< eof_gap_y old_eof_gap_y)
								(and (= eof_gap_y old_eof_gap_y) (< eof_gap_x old_eof_gap_x)))
							(not (some! *cursor_y* -1 t (const blank-line?)
								(list (. buffer :get_text_lines)))))
					(each (lambda (action)
							(eval action)
							(bind '(x y) (. buffer :get_cursor))
							(setq *cursor_x* x *cursor_y* y))
						*macro_actions*)
					(setq old_eof_gap_x eof_gap_x old_eof_gap_y eof_gap_y
						eof_gap_x (- (length (. buffer :get_text_line *cursor_y*)) *cursor_x*)
						eof_gap_y (- (elem 1 (. buffer :get_size)) *cursor_y*)))))
		(clear-selection) (refresh)))
