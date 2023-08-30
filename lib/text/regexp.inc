;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

(import "./substr.inc")

;module
(env-push)

(defun match (text creg start)
	; (match text creg start) -> :nil | end
	;for each char in text
	(and (defq current (list 0) next (list) end (some! start -1 :nil
		(lambda (s)
			;for each current state
			(each (lambda (index)
					(defq state (elem-get index creg))
					(when (eval (elem-get 0 state))
						;matched text char, so insert next states
						(each! 1 -1 (# (unless (find %0 next) (push next %0))) (list state))))
				current)
			;swap to new states
			(defq tmp current)
			(setq current next next (clear tmp))
			(cond
				;break out if no states
				((= (length current) 0) -1)
				;break out if any end states
				((find (length creg) current) (inc _))))
		(list text))) (> end -1) end))

(defclass Regexp () (Substr)
	; (Regexp) -> regexp

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq creg (. meta_cache :find pattern))
			(cond
				((= (length pattern) 0)
					(defq creg :nil))
				(:t	(defq creg (list))
					(each (# (push creg `((,eql s ,%0) ,(+ (length creg) 1)))) pattern)))
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
