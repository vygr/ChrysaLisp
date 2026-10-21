;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(clip-put-rpc "cut data"))

(defun action-copy ()
	(clip-put-rpc "copy data"))

(defun action-paste ()
	(unless (eql (defq data (clip-get-rpc)) "")
		;do stuff with data
		))
