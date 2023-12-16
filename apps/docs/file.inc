;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(cond
		((starts-with "```" line)
			:text)
		(:t (bind '(file &optional start_exp end_exp)
				(first (read (string-stream (cat "(" line ")")))))
			;collect file lines
			(defq lines (list))
			;skip to start
			(defq stream (file-stream file))
			(while (and start_exp (defq line (read-line stream)))
				(when (found? line start_exp)
					(push lines line)
					(setq start_exp :nil)))
			;skip to end
			(cond
				((eql end_exp "")
					(while (and end_exp (defq line (read-line stream)))
						(if (or (eql line "") (eql line (ascii-char 13)))
							(setq end_exp :nil)
							(push lines line))))
				(end_exp
					(while (and end_exp (defq line (read-line stream)))
						(if (found? line end_exp)
							(setq end_exp :nil)
							(push lines line))))
				(:t (while (defq line (read-line stream))
						(push lines line))))
			;trim and left align
			(defq margin +max_int lines (map
				(# (and (> margin 0) (defq m (find "(" %0))
						(setq margin (min m margin)))
					(trim-end %0 (ascii-char 13))) lines))
			(and (/= margin 0) (/= margin +max_int)
				(setq lines (map
					(# (if (> (length %0) margin) (slice margin -1 %0) %0)) lines)))
			(while (eql (last lines) "") (pop lines))
			(each (# ((handler-func :vdu) :vdu page %0)) lines)
			((handler-func :vdu) :vdu page "```")
			state)))

;module
(export-symbols '(handler))
(env-pop)
