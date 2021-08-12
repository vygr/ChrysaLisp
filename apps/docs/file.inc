;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			:text)
		(t	(each-line (lambda (line)
					(timeslice)
					((handler-func :vdu) :vdu page (trim-end line (ascii-char 13))))
				(file-stream line))
			((handler-func :vdu) :vdu page "```")
			state)))

;module
(export (penv)
	handler)
(env-pop)
