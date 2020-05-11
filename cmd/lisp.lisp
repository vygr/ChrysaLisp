;imports
(import 'cmd/asm.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: lisp [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then will REPL from stdin.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and (defq stdio (create-stdio)) (defq args (options stdio usage)))
		(defq stdin (file-stream 'stdin) stdout (file-stream 'stdout) stderr (file-stream 'stderr))
		(if (<= (length args) 1)
			;run asm.inc, and print sign on
			(progn
				(print "ChrysaLisp 1.3")
				(print "Press ESC/Enter to exit.")
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
				(read-char stdin)))))
