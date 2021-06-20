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
		(defq :macro_playback t)
		(undoable
			(each (lambda ((action &rest params))
					(apply action params)
					(bind '(x y) (. buffer :get_cursor))
					(setq *cursor_x* x *cursor_y* y))
				*macro_actions*))
		(undef (env) :macro_playback)
		(clear-selection) (refresh)))

(defun action-macro-to-eof ()
	(when (> (length *macro_actions*) 0)
		(defq :macro_playback t)
		(undoable
			(bind '(w h) (. buffer :get_size))
			(defq eof_gap (- h *cursor_y*) old_eof_gap +max_int)
			(while (and (> eof_gap 0) (< eof_gap old_eof_gap))
				(each (lambda ((action &rest params))
						(apply action params)
						(bind '(x y) (. buffer :get_cursor))
						(setq *cursor_x* x *cursor_y* y))
					*macro_actions*)
				(bind '(w h) (. buffer :get_size))
				(setq old_eof_gap eof_gap eof_gap (- h *cursor_y*))))
		(undef (env) :macro_playback)
		(clear-selection) (refresh)))
