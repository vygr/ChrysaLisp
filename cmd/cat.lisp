;import settings
(run 'apps/cmd.lisp)

;cat a file to stdout
(defun cat-file (_)
	(if (setq _ (file-stream _))
		(while (defq c (read-char _))
			(prin (char c)))
		(slot write_flush stdout)))

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) stdout (file-stream 'stdout))
	(if (le (length (defq args (slot get_args slave))) 1)
		;cat from stdin
		(while (defq l (read-line stdin))
			(cat-file l))
		;cat from args
		(each cat-file (slice 1 -1 args))))
