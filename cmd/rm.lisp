(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: rm [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then paths are read from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;rm from stdin
			(lines! pii-remove (io-stream 'stdin))
			;rm from args
			(each pii-remove (rest args)))))
