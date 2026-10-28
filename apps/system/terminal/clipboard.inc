;;;;;;;;;;;;;;;;;;;
; clipboard actions
;;;;;;;;;;;;;;;;;;;

(import "apps/tools/edit/clipboard.inc")

(redefun action-paste ()
	(unless (eql (defq data (clip-get-rpc)) "")
		(when (> (length (defq lines (split data "\n"))) 0)
			(each! (# (action-insert %0) (action-break)) (list lines) 0 -2)
			(action-insert (pop lines))
			(if (ends-with "\n" data)
				(action-break)))))
