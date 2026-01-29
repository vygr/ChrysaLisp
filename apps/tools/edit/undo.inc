;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(. *edit* :undo)
	(refresh))

(defun action-redo ()
	(. *edit* :redo)
	(refresh))

(defun action-rewind ()
	(. *edit* :rewind)
	(refresh))

(defun action-undo-global ()
	(for-all-buffers (action-undo)))

(defun action-redo-global ()
	(for-all-buffers (action-redo)))
