;import settings
(run 'apps/cmd.lisp)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream '#0) lines (list))
	(if (le (length (defq args (slot get_args slave))) 1)
		;from stdin
		(while (defq l (read-line stdin))
			(cond
				((eq (length lines) 0)
					(push lines l))
				((not (eql (elem -2 lines) l))
					(push lines l))))
		;from args
		(each (lambda (l)
			(cond
				((eq (length lines) 0)
					(push lines l))
				((not (eql (elem -2 lines) l))
					(push lines l)))) (slice 1 -1 args)))
	(each print lines))
