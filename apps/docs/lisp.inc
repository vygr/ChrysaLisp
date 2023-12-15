;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			(each (# (task-slice) ((handler-func :vdu) :vdu page %0)) lines)
			((handler-func :vdu) :vdu page "```")
			(each (# (task-slice) ((handler-func :lispq) :lispq page %0)) lines)
			((handler-func :lispq) :lispq page "```")
			(clear lines)
			:text)
		(:t (push lines (trim-end line (ascii-char 13)))
			state)))

;module
(export-symbols '(handler))
(env-pop)
