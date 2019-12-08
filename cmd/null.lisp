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

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq args (options slave usage)))
	(defq stdin (file-stream 'stdin))
	(while (read-char stdin)))
