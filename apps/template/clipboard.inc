;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(defun action-cut ()
	(clipboard-put "cut data"))

(defun action-copy ()
	(clipboard-put "copy data"))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		;do stuff with data
		))
