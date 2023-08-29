;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

(import "./substr.inc")

(defclass Regexp () (Substr)
	; (Regexp) -> regexp

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(.super this pattern))

	(defmethod :search (this text pattern &optional creg)
		; (. regexp :search text pattern &optional meta) -> indexes
		(.super this text pattern &optional creg))
	)
