(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: echo [options] arg ...

	options:
		-h --help: this help info.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(print (join (rest args) " "))))
