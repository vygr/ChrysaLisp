;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

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
	(when (and (defq slave (create-slave)) (defq args (options slave usage)))
		(defq stdin (file-stream 'stdin) lines (list))
		(if (<= (length args) 1)
			;shuffle stdin
			(while (defq l (read-line stdin))
				(push lines l))
			;shuffle args
			(setq lines (slice 1 -1 args)))
		(each print (shuffle lines))))
