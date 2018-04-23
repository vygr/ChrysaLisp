;import settings
(run 'apps/cmd.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) lines (list))
	(if (le (length (defq args (slot get_args slave))) 1)
		;sort stdin
		(while (defq l (read-line stdin))
			(push lines l))
		;sort args
		(setq lines (slice 1 -1 args)))
	(each print (shuffle lines)))
