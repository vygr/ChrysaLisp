;module
(env-push)

(defun output (line)
	((handler-func :vdu) :vdu page (trim-end line (ascii-char 13))))

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			:text)
		(:t (bind '(file &optional start_exp end_exp)
				(first (read (string-stream (cat "(" line ")")))))
			;skip to start
			(defq stream (file-stream file))
			(while (and start_exp (defq line (read-line stream)))
				(when (found? line start_exp)
					(output line)
					(setq start_exp :nil)))
			;show to end
			(cond
				((eql end_exp "")
					(while (and end_exp (defq line (read-line stream)))
						(if (eql line "")
							(setq end_exp :nil)
							(output line))))
				(end_exp
					(while (and end_exp (defq line (read-line stream)))
						(if (found? line end_exp)
							(setq end_exp :nil)
							(output line))))
				(:t (while (defq line (read-line stream))
						(output line))))
			((handler-func :vdu) :vdu page "```")
			state)))

;module
(export-symbols '(handler))
(env-pop)
