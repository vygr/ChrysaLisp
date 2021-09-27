;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(. (. *edit* :get_buffer) :undo)
	(clear-selection) (refresh))

(defun action-redo ()
	(. (. *edit* :get_buffer) :redo)
	(clear-selection) (refresh))

(defun action-rewind ()
	(defq buffer (. *edit* :get_buffer))
	(while (> (length (get :undo_stack buffer)) 0)
		(. buffer :undo))
	(clear-selection) (refresh))
