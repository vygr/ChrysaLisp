;imports
(import 'class/lisp.inc)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream 'stdin) stdout (file-stream 'stdout) stderr (file-stream 'stderr))
	(if (<= (length (defq args (slave-get-args slave))) 1)
		;run asm.inc, and print sign on
		(progn
			(print "ChrysaLisp 1.3")
			(print "Press ESC/Enter to exit.")
			(import 'cmd/asm.inc)
			(stream-flush stdout)
			(stream-flush stderr))
		;else, include any files given as args (in this enviroment, hence the while loop !)
		(progn
			(defq i 0)
			(while (< (setq i (inc i)) (length args))
				(import (elem i args))
				(stream-flush stdout)
				(stream-flush stderr))))
	;repl from stdin
	(while (catch (repl stdin 'stdin) t)
		(stream-flush stdout)
		(stream-flush stderr)
		(while (/= (stream-avail stdin) 0)
			(read-char stdin))))
