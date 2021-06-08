;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(. *current_buffer* :undo)
	(clear-selection) (refresh))

(defun action-redo ()
	(. *current_buffer* :redo)
	(clear-selection) (refresh))

(defun action-rewind ()
	(while (> (length (get :undo_stack *current_buffer*)) 0)
		(. *current_buffer* :undo))
	(clear-selection) (refresh))
