;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(clip-put-rpc (.-> *edit* :get_buffer :cut))
	(refresh))

(defun action-copy ()
	(clip-put-rpc (.-> *edit* :get_buffer :copy))
	(refresh))

(defun action-paste ()
	(.-> *edit* :get_buffer (:paste (clip-get-rpc)))
	(refresh))

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
