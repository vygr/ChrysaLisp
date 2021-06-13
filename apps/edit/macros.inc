;;;;;;;;;;;;;;;
; macro actions
;;;;;;;;;;;;;;;

(defun action-macro-record ()
	(setq *macro_record* (not *macro_record*))
	(cond
		(*macro_record*
			(radio-select macro_toolbar 1)
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
