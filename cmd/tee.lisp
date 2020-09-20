;imports
(import 'class/lisp.inc)
(import 'lib/options/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: tee [options] [path] ...
	options:
		-h --help: this help info.
	Read from stdin, write to stdout and all given paths.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and (defq stdio (create-stdio)) (defq args (options stdio usage)))
		(defq stdin (io-stream 'stdin) buffer (string-stream (cat "")))
		(while (defq c (read-char stdin))
			(write buffer (prin (char c))))
		(setq buffer (str buffer))
		(each (# (save buffer %0)) (slice 1 -1 args))))
