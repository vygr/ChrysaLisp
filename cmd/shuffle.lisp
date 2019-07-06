;imports
(import 'class/lisp.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) lines (list))
	(if (le (length (defq args (slave-get-args slave))) 1)
		;shuffle stdin
		(while (defq l (read-line stdin))
			(push lines l))
		;shuffle args
		(setq lines (slice 1 -1 args)))
	(each print (shuffle lines)))
