;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: sort [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will sort lines from stdin.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq lines (list))
		(if (<= (length args) 1)
			;sort stdin
			(each-line (lambda (_) (push lines _)) (file-stream 'stdin))
			;sort args
			(setq lines (slice 1 -1 args)))
		(each print (sort cmp lines))))
