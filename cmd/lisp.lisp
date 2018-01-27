;import settings
(run 'apps/cmd.lisp)

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq stdin (file-stream '#0) stdout (file-stream '#1) stderr (file-stream '#2))
	(if (le (length (defq args (slot get_args slave))) 1)
		;run asm.inc, and print sign on
		(progn
			(run 'cmd/asm.inc)
			(print "ChrysaLisp 1.2")
			(print "Press ESC/Enter to exit.")
			(slot write_flush stdout)
			(slot write_flush stderr))
		;else, run any files given as args (in this enviroment, hence the while loop !)
		(progn
			(run 'cmd/asm.inc)
			(defq i 0)
			(while (lt (setq i (inc i)) (length args))
				(run (elem i args))
				(slot write_flush stdout)
				(slot write_flush stderr))))
	;repl from stdin
	(while (catch (repl stdin 'stdin))
		(slot write_flush stdout)
		(slot write_flush stderr)
		(while (ne (slot available stdin) 0)
			(read-char stdin))))
