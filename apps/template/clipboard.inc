;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(debug-brk "cut" :t)
	(clipboard-put "cut data"))

(defun action-copy ()
	(debug-brk "copy" :t)
	(clipboard-put "copy data"))

(defun action-paste ()
	(debug-brk "paste" :t)
	(unless (eql (defq data (clipboard-get)) "")
		;do stuff with data
		))
