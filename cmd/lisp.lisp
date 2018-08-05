;import settings
(run 'cmd/lisp.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) stdout (file-stream 'stdout) stderr (file-stream 'stderr))
	(if (le (length (defq args (slave-get-args slave))) 1)
		;run asm.inc, and print sign on
		(progn
			(print "ChrysaLisp 1.3")
			(print "Press ESC/Enter to exit.")
			(run 'cmd/asm.inc)
			(stream-write-flush stdout)
			(stream-write-flush stderr))
		;else, run any files given as args (in this enviroment, hence the while loop !)
		(progn
			(defq i 0)
			(while (lt (setq i (inc i)) (length args))
				(run (elem i args))
				(stream-write-flush stdout)
				(stream-write-flush stderr))))
	;repl from stdin
	(while (catch (repl stdin 'stdin) t)
		(stream-write-flush stdout)
		(stream-write-flush stderr)
		(while (ne (stream-available stdin) 0)
			(read-char stdin))))
