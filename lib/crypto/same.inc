;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Two strs compared in the same time whether they are the same or
; not, every byte looked at: a tag or a proof checked with (eql)
; stops at the first byte that differs, and how long it took says how
; much of a guess was right.
;
; (crypto-same? a b) -> :nil | :t
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crypto-same? (a b)
	; (crypto-same? a b) -> :nil | :t
	(when (= (length a) (length b))
		(defq differ 0)
		(each (# (setq differ (logior differ (logxor (code %0) (code %1))))) a b)
		(= differ 0)))
