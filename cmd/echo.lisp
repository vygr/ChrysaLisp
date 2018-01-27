;import settings
(run 'apps/cmd.lisp)

;initialize pipe details and command args, abort on error
(if (defq slave (create-slave))
	(each print (slice 1 -1 (slot get_args slave))))
