(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: tee [options] [path] ...
	options:
		-h --help: this help info.
	Read from stdin, write to stdout and all given paths.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and (defq stdio (create-stdio)) (defq args (options stdio usage)))
		(defq stdin (io-stream 'stdin) files (map (# (file-stream %0 +file_open_write)) (slice 1 -1 args)))
		(while (defq c (read-char stdin))
			(prin (char c))
			(each (# (write-char %0 c)) files))))
