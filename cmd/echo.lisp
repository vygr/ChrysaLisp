;import settings
(include 'class/lisp.inc)

;initialize pipe details and command args, abort on error
(if (defq slave (create-slave))
	(each print (slice 1 -1 (slave-get-args slave))))
