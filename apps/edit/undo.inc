;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(. (. *edit* :get_buffer) :undo)
	(. *edit* :clear_selection)
	(refresh))

(defun action-redo ()
	(. (. *edit* :get_buffer) :redo)
	(. *edit* :clear_selection)
	(refresh))

(defun action-rewind ()
	(defq buffer (. *edit* :get_buffer))
	(while (> (length (get :undo_stack buffer)) 0)
		(. buffer :undo))
	(. *edit* :clear_selection)
	(refresh))
