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
	(.-> *edit* :get_buffer :rewind)
	(refresh))

(defun action-undo-global ()
	(for-all-buffers (action-undo)))

(defun action-redo-global ()
	(for-all-buffers (action-redo)))
