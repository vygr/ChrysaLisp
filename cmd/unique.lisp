;import settings
(run 'apps/cmd.lisp)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin))
	(if (le (length (defq args (slot get_args slave))) 1)
		;from stdin
		(when (defq ll (read-line stdin))
			(print ll)
			(while (defq nl (read-line stdin))
				(unless (eql ll nl)
					(print (setq ll nl)))))
		;from args
		(progn
			(print (defq ll (elem 1 args)))
			(each (lambda (nl)
				(unless (eql ll nl)
					(print (setq ll nl)))) (slice 2 -1 args)))))
