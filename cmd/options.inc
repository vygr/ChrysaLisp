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
	(defun arg-action (arg &optional arg2)
		(cond
			((defq opt_entry (options-find optlist arg))
				;found an entry
				(cond
					((str? (elem 1 opt_entry))
						;just print it and quit
						(options-print (elem 1 opt_entry))
						(setq state nil))
					((eql state 'b)
						;found an entry in state 'b, call 2 arg handler
						((elem 1 opt_entry) arg arg2)
						(setq state 'a))
					(t	;else call 1 arg handler
						((elem 1 opt_entry) arg))))
			(t	;not found entry, print help and quit
				(options-print (elem 1 (elem 0 optlist)))
				(setq state nil))))
	;scan the slave args and process acording to the optlist
	(defq args (slave-get-args slave) out_args (list) state 'a last_arg "")
	(each (lambda (arg)
		(case state
			(a
				;look for "-" or "+", pass through if not
				(cond
					((starts-with "-" arg)
						(arg-action arg))
					((starts-with "+" arg)
						(setq state 'b last_arg arg))
					(t	(push out_args arg))))
			(b	;look up last_arg in optlist
				(arg-action last_arg arg)))) args)
	(if state out_args))
