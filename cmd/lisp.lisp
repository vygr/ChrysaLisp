(import "lib/options/options.inc")
(import "lib/asm/asm.inc")

(defq usage `(
(("-h" "--help")
"Usage: lisp [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then will REPL from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq stdin (io-stream 'stdin) stdout (io-stream 'stdout) stderr (io-stream 'stderr))
		(if (<= (length args) 1)
			;run asm.inc, and print sign on
			(progn
				(print "ChrysaLisp")
				(print "Press ESC/Enter to exit.")
				(stream-flush stdout)
				(stream-flush stderr))
			;else, include any files given as args (in this environment, hence the while loop !)
			(progn
				(defq i 0)
				(while (< (++ i) (length args))
					(import (elem-get args i))
					(stream-flush stdout)
					(stream-flush stderr))))
		;repl from stdin
		(while (catch (repl stdin 'stdin) :t)
			(stream-flush stdout)
			(stream-flush stderr)
			(while (/= (stream-avail stdin) 0)
				(read-char stdin)))))
