;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(.-> *edit* :get_buffer :undo)
	(refresh))

(defun action-redo ()
	(.-> *edit* :get_buffer :redo)
	(refresh))

(defun action-rewind ()
	(defq buffer (. *edit* :get_buffer))
	(while (> (length (get :undo_stack buffer)) 0) (. buffer :undo))
	(refresh))

(defun action-undo-global ()
	(for-all-buffers (action-undo)))

(defun action-redo-global ()
	(for-all-buffers (action-redo)))
