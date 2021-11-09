;;;;;;;;;;;;;;;;;;;;
; options prosessing
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun options-print (&rest _)
	; (options-print &rest _)
	(stream-flush (write (io-stream 'stdout)
		(apply str (cat _ (list (ascii-char 10)))))))

(defun options-find (optlist arg)
	; (options-find optlist arg) -> nil | opt_entry
	(some (lambda (opt_entry)
		(if (find arg (elem-get 0 opt_entry)) opt_entry)) optlist))

(defun options (stdio optlist)
	; (options stdio optlist) -> nil | args
	;scan the stdio args and process acording to the optlist
	(defq args (stdio-get-args stdio) out_args (list))
	(while (/= (length args) 0)
		;look for "-", pass through if not
		(defq arg (elem-get 0 args) args (slice 1 -1 args))
		(if (starts-with "-" arg)
			(if (defq opt_entry (or
					(options-find optlist arg)
					(options-find optlist "-h")))
				;found an entry, else help
				(cond
					((str? (elem-get 1 opt_entry))
						;just print it and quit
						(options-print (elem-get 1 opt_entry))
						(setq args '() out_args nil))
					(t	;else call arg handler
						(setq args ((elem-get 1 opt_entry) args arg))))
				;error, so quit
				(setq args '() out_args nil))
			;arg to output
			(push out_args arg))) out_args)

;module
(export-symbols
	options)
(env-pop)
