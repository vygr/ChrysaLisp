(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: nodes [options]
	options:
		-h --help: this help info.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq prefix (if (> (length args) 1) (elem-get 1 args) ""))
		(each (# (print (to-service-id %0))) (mail-nodes))))
