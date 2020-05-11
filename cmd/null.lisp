;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: null [options]
	options:
		-h --help: this help info.")
(("-e" "--example")
	,(bind-fun (lambda (o) (options-print "handler for: " o))))
(("+x" "++xtraarg")
	,(bind-fun (lambda (o a) (options-print "handler for: " o " " a))))
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and (defq stdio (create-stdio)) (defq args (options stdio usage)))
		(defq stdin (file-stream 'stdin))
		(while (read-char stdin))))
