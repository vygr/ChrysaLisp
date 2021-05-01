;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			((handler-func :vdu) :vdu page line))
		(t	(each-line (lambda (line)
					(task-sleep 0)
					((handler-func :vdu) :vdu page (trim-end line (ascii-char 13))))
				(file-stream line))
			state)))

;module
(export (penv)
	handler)
(env-pop)
