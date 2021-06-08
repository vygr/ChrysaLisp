;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(clipboard-put (. *current_buffer* :cut *anchor_x* *anchor_y*))
	(clear-selection) (refresh))

(defun action-copy ()
	(clipboard-put (. *current_buffer* :copy *anchor_x* *anchor_y*))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(undoable
			(. buffer :cut *anchor_x* *anchor_y*)
			(. buffer :paste data))
		(clear-selection) (refresh)))

(defun action-cut-word ()
	(action-select-word)
	(action-cut))

(defun action-cut-line ()
	(action-select-line)
	(action-cut))

(defun action-cut-paragraph ()
	(action-select-paragraph)
	(action-cut))

(defun action-cut-block ()
	(action-select-block)
	(action-cut))
