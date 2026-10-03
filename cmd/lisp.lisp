(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: lisp [options] [path] ...

    options:
        -h --help: this help info.
        -r --repl ...: read code from remainder of command line into REPL.

    If no paths given on command line
    then will REPL from stdin.")
(("-r" "--repl") ,(static-qq (lambda (args arg) (setq opt_r (join args " ")) '())))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_r :nil args (options stdio usage)))
		(defq stdin (io-stream 'stdin) stdout (io-stream 'stdout) stderr (io-stream 'stderr))
		(when (> (length args) 1)
			;include any files given as args (in this environment, hence the while loop !)
			(defq i 0)
			(while (< (++ i) (length args))
				(import (elem-get args i))
				(stream-flush stdout)
				(stream-flush stderr)))
		(cond
			(opt_r
				;repl from command line string
				(defq res (repl (string-stream opt_r) 'repl))
				(when (find :error (type-of res))
					(print res))
				(stream-flush stdout)
				(stream-flush stderr))
			(:t
				(when (<= (length args) 1)
					;run asm.inc, and print sign on
					(print "ChrysaLisp")
					(print "Press ESC/Enter to exit.")
					(stream-flush stdout)
					(stream-flush stderr))
				;repl from stdin
				(while (catch (repl stdin 'stdin) :t)
					(stream-flush stdout)
					(stream-flush stderr)
					(while (/= (stream-avail stdin) 0) (read-blk stdin 1024)))))))
