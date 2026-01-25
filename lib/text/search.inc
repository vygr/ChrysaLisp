;;;;;;;;;;;;;;;
; search object
;;;;;;;;;;;;;;;

(defclass Search () :nil
	; (Search) -> search
	(defabstractmethod :compile (pattern))
		; (. search :compile pattern) -> :nil | meta
	(defabstractmethod :search (text meta))
		; (. search :search text meta) -> matches
	(defabstractmethod :match? (text meta))
		; (. search :match? text meta) -> :t | :nil
	)
