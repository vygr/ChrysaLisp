;;;;;;;;;;;;;;;;
; cursor actions
;;;;;;;;;;;;;;;;

(defun action-home ()
	(. *current_buffer* :set_cursor 0 *cursor_y*)
	(clear-selection) (refresh))

(defun action-end ()
	(bind '(x y) (. *current_buffer* :constrain +max_int *cursor_y*))
	(. *current_buffer* :set_cursor x y)
	(clear-selection) (refresh))

(defun action-left ()
	(cond
		((selection?)
			(bind '(sx sy sx1 sy1) (sort-selection))
			(. *current_buffer* :set_cursor sx sy))
		(t  (. *current_buffer* :left)))
	(clear-selection) (refresh))

(defun action-right ()
	(cond
		((selection?)
			(bind '(sx sy sx1 sy1) (sort-selection))
			(. *current_buffer* :set_cursor sx1 sy1))
		(t  (. *current_buffer* :right)))
	(clear-selection) (refresh))

(defun action-up ()
	(when (selection?)
		(bind '(sx sy sx1 sy1) (sort-selection))
		(. *current_buffer* :set_cursor sx sy))
	(. *current_buffer* :up)
	(clear-selection) (refresh))

(defun action-down ()
	(when (selection?)
		(bind '(sx sy sx1 sy1) (sort-selection))
		(. *current_buffer* :set_cursor sx1 sy1))
	(. *current_buffer* :down)
	(clear-selection) (refresh))

(defun action-left-bracket ()
	(when (bind '(x y) (. *current_buffer* :left_bracket))
		(. *current_buffer* :set_cursor x y)
		(clear-selection) (refresh)))

(defun action-right-bracket ()
	(when (bind '(x y) (. *current_buffer* :right_bracket))
		(. *current_buffer* :set_cursor x y)
		(clear-selection) (refresh)))

(defun action-home-select ()
	(. *current_buffer* :set_cursor 0 *cursor_y*)
	(refresh))

(defun action-end-select ()
	(bind '(x y) (. *current_buffer* :constrain +max_int *cursor_y*))
	(. *current_buffer* :set_cursor x y)
	(refresh))

(defun action-left-select ()
	(. *current_buffer* :left)
	(refresh))

(defun action-right-select ()
	(. *current_buffer* :right)
	(refresh))

(defun action-up-select ()
	(. *current_buffer* :up)
	(refresh))

(defun action-down-select ()
	(. *current_buffer* :down)
	(refresh))
