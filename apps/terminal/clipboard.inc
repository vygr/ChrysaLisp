;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(import "././edit/clipboard.inc")

(redefun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(when (> (length (defq lines (split data (ascii-char +char_lf)))) 0)
			(each! (# (action-insert %0) (action-break)) (list lines) 0 -2)
			(action-insert (pop lines))
			(if (ends-with (ascii-char +char_lf) data)
				(action-break)))))
