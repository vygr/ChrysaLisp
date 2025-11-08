;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(debug-brk "cut")
	(clip-put-rpc "cut data"))

(defun action-copy ()
	(debug-brk "copy")
	(clip-put-rpc "copy data"))

(defun action-paste ()
	(debug-brk "paste")
	(unless (eql (defq data (clip-get-rpc)) "")
		;do stuff with data
		))
