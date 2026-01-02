;module
(env-push)

(defun redirect (state lines)
	(each (# (task-slice) ((handler-func state) state page %0)) lines)
	((handler-func state) state page "```"))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((match? line "^\s*```")
			(redirect :vdu lines)
			(redirect :lispq lines)
			(clear lines)
			:text)
		(:t (push lines line)
			state)))

;module
(export-symbols '(handler))
(env-pop)
