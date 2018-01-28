;import settings
(run 'apps/cmd.lisp)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream '#0) lines (list))
	(if (le (length (defq args (slot get_args slave))) 1)
		;from stdin
		(when (defq l (read-line stdin))
			(push lines l)
			(while (setq l (read-line stdin))
				(unless (eql (elem -2 lines) l)
					(push lines l))))
		;from args
		(progn
			(push lines (elem 1 args))
			(each (lambda (l)
				(unless (eql (elem -2 lines) l)
					(push lines l))))))
	(each print lines))
