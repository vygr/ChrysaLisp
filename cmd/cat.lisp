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
	(defq stdin (file-stream '#0) stdout (file-stream '#1))
	(if (le (length (defq args (slot get_args slave))) 1)
		;cat from stdin
		(while (defq l (read-line stdin))
			(cat-file l)
			(slot write_flush stdout))
		;cat from args
		(each cat-file (slice 1 -1 args))))
