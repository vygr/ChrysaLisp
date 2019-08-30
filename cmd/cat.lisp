;imports
(import 'class/lisp.inc)

;cat a file to stdout
(defun cat-file (_)
	(if (setq _ (file-stream _))
		(while (defq c (read-char _))
			(prin (char c)))
		(stream-write-flush stdout)))

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) stdout (file-stream 'stdout))
	(if (<= (length (defq args (slave-get-args slave))) 1)
		;cat from stdin
		(while (defq l (read-line stdin))
			(cat-file l))
		;cat from args
		(each cat-file (slice 1 -1 args))))
