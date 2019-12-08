;;;;;;;;;;;;;;;;;;;;
; options prosessing
;;;;;;;;;;;;;;;;;;;;

(defun-bind options-print (&rest _)
	;(options-print &rest _)
	(stream-flush (write (file-stream 'stderr)
		(apply str (cat _ (list (ascii-char 10)))))))

(defun-bind options-find (optlist arg)
	;(options-find optlist arg) -> nil|opt_entry
	(some (lambda (opt_entry)
		(if (some (lambda (_)
				(eql arg _)) (elem 0 opt_entry)) opt_entry)) optlist))

(defun-bind options (slave optlist)
	;(options slave optlist) -> nil|args
	(defq args (slave-get-args slave) out_args (list) state 'a last_arg "")
	(each (lambda (arg)
		(case state
			(a
				;look for "-" or "+", pass through if not
				(cond
					((starts-with "-" arg)
						(cond
							((defq opt_entry (options-find optlist arg))
								;found an entry
								(cond
									((str? (elem 1 opt_entry))
										;just print it and quit
										(options-print (elem 1 opt_entry))
										(setq state nil))
									(t	;else call handler
										((elem 1 opt_entry) arg))))
							(t	;not found entry, print help and quit
								(options-print (elem 1 (elem 0 optlist)))
								(setq state nil))))
					((starts-with "+" arg)
						(setq state 'b last_arg arg))
					(t	(push out_args arg))))
			(b	;look up last_arg in optlist
				(cond
					((defq opt_entry (options-find optlist last_arg))
						;found an entry, call handler
						((elem 1 opt_entry) last_arg arg)
						(setq state 'a))
					(t	;not found entry, print help and quit
						(options-print (elem 1 (elem 0 optlist)))
						(setq state nil)))))) args)
	(if state out_args))
