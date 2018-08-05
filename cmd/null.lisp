;import settings
(run 'cmd/lisp.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin))
	(while (read-char stdin)))
