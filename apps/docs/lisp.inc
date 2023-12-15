;module
(env-push)

(defun redirect (state lines)
	(each (# (task-slice) ((handler-func state) state page %0)) lines)
	((handler-func state) state page "```"))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			(redirect :vdu lines)
			(redirect :lispq lines)
			(clear lines)
			:text)
		(:t (push lines (trim-end line (ascii-char 13)))
			state)))

;module
(export-symbols '(handler))
(env-pop)
