(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: null [options]

	options:
		-h --help: this help info.")
(("-e" "--example")
	,(lambda (args arg)
		(options-print "handler for: " arg) args))
(("-x" "--xtra")
	,(lambda (args arg)
		(options-print "handler for: " arg " " (first args))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq stdin (io-stream 'stdin))
		(while (read-blk stdin 1024))))
