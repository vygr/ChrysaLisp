;imports
(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: shuffle [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will shuffle lines from stdin.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq lines (list))
		(if (<= (length args) 1)
			;shuffle stdin
			(each-line (# (push lines %0)) (io-stream 'stdin))
			;shuffle args
			(setq lines (slice 1 -1 args)))
		(each print (shuffle lines))))
