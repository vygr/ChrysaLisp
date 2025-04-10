;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(bind '(ax ay) (. *edit* :get_anchor))
	(clip-put-rpc (.-> *edit* :get_buffer (:cut ax ay)))
	(. *edit* :clear_selection)
	(refresh))

(defun action-copy ()
	(bind '(ax ay) (. *edit* :get_anchor))
	(clip-put-rpc (.-> *edit* :get_buffer (:copy ax ay)))
	(refresh))

(defun action-paste ()
	(unless (eql (defq data (clip-get-rpc)) "")
		(defq buffer (. *edit* :get_buffer))
		(undoable
			(bind '(ax ay) (. *edit* :get_anchor))
			(. buffer :cut ax ay)
			(. buffer :paste data))
		(. *edit* :clear_selection)
		(refresh)))

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

(defun action-copy-word ()
	(action-select-word)
	(action-copy))

(defun action-copy-line ()
	(action-select-line)
	(action-copy))

(defun action-copy-paragraph ()
	(action-select-paragraph)
	(action-copy))

(defun action-copy-block ()
	(action-select-block)
	(action-copy))
