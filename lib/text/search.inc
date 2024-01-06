;;;;;;;;;;;;;;;
; search object
;;;;;;;;;;;;;;;

(defclass Search () :nil
	; (Search) -> search
	(defabstractmethod :compile (pattern))
		; (. search :compile pattern) -> :nil | meta
	(defabstractmethod :search (text pattern &optional meta))
		; (. search :search text pattern [meta]) -> matches
	(defabstractmethod :match? (text pattern &optional meta))
		; (. search :match? text pattern [meta]) -> :t | :nil
	)
