;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(debug-brk "cut" :t)
	(clip-put-rpc "cut data"))

(defun action-copy ()
	(debug-brk "copy" :t)
	(clip-put-rpc "copy data"))

(defun action-paste ()
	(debug-brk "paste" :t)
	(unless (eql (defq data (clip-get-rpc)) "")
		;do stuff with data
		))
