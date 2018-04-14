;import settings
(run 'apps/cmd.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream '#0))
	(while (read-char stdin)))
