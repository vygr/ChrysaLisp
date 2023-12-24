;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(import "././edit/clipboard.inc")

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(when (> (length (defq lines (split data (ascii-char +char_lf)))) 0)
			(each! 0 -2 (# (action-insert %0) (action-break)) (list lines))
			(action-insert (pop lines))
			(if (ends-with (ascii-char +char_lf) data)
				(action-break)))))
