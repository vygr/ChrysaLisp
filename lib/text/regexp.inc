;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

(import "./substr.inc")

;module
(env-push)

(defun match-literal (c) (eql c s))

(defun match (text creg start)
	(defq current (list 0) next (list))
	;for each char in text
	(some! start -1 :nil (lambda (s)
			;for each current state
			(each (lambda (index)
					(defq state (elem-get index creg))
					(when (eval (elem-get 0 state))
						;matched text char, so insert next states
						(each (# (unless (find %0 next) (push next %0)))
							(slice 1 -1 state))))
				current)
			;swap to new states
			(defq tmp current)
			(setq current next next (clear tmp))
			;any end states ?
			(if (some (# (= %0 (length creg))) current) (inc _)))
		(list text)))

(defclass Regexp () (Substr)
	; (Regexp) -> regexp

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq creg (. meta_cache :find pattern))
			(defq creg (list))
			(each (# (push creg (list (list (const match-literal) %0) (inc (length creg))))) pattern)
			(. meta_cache :insert pattern creg))
		creg)

	(defmethod :search (this text pattern &optional creg)
		; (. regexp :search text pattern &optional meta) -> indexes
		(defq out (list) creg (opt creg (. this :compile pattern)))
		(when creg
			(defq x -1)
			(while (< (setq x (inc x)) (length text))
				(when (defq x1 (match text creg x))
					(push out (list x x1))
					(setq x (dec x1)))))
		out)
	)

;module
(export-classes
	Regexp)
(env-pop)
