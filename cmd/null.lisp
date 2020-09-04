;imports
(import 'class/lisp.inc)
(import 'lib/options/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: null [options]
	options:
		-h --help: this help info.")
(("-e" "--example")
	,(prebind (lambda (args arg)
		(options-print "handler for: " arg) args)))
(("-x" "--xtra")
	,(prebind (lambda (args arg)
		(options-print "handler for: " arg " " (elem 0 args))
		(slice 1 -1 args))))
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq stdin (file-stream 'stdin))
		(while (read-char stdin))))
