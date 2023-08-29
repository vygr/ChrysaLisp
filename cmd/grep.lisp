(import "class/lisp.inc")
(import "lib/options/options.inc")
(import "lib/text/regexp.inc")

;grep a stream to stdout
(defun grep-file (stream)
	(each-line (# (if (nempty? (. regexp :search %0 pattern cexp)) (print %0)))
		stream))

(defq usage `(
(("-h" "--help")
"Usage: grep [options] [path] ...
	options:
		-h --help: this help info.
		-e --exp pattern: regular expression.
	If no paths given on command line
	then will grep stdin.")
(("-e" "--exp")
	,(lambda (args arg)
		(setq pattern (elem-get 0 args))
		(slice 1 -1 args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq pattern "" args (options stdio usage))
			(defq regexp (Regexp) cexp (. regexp :compile pattern)))
		(if (<= (length args) 1)
			;grep from stdin
			(grep-file (io-stream 'stdin))
			;grep from args as files
			(each (# (grep-file (file-stream %0))) (slice 1 -1 args)))))
