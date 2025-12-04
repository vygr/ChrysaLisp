(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: yes [string]

	options:
		-h --help: this help info.

	Repeatedly output a line with STRING (default 'y') until killed.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq msg (if (<= (length args) 1)
			"y"
			(join (rest args) " ")))
		(while :t
			(print msg))))
