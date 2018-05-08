;import settings
(run 'apps/cmd.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) buffer (string-stream (cat "")))
	(while (defq c (read-char stdin))
		(write buffer (prin (char c))))
	(setq buffer (str buffer))
	(each (lambda (_)
		(save buffer _)) (slice 1 -1 (slot get_args slave))))
