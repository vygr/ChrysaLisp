;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

;cat a file to stdout
(defun cat-file (_)
	(if (setq _ (file-stream _))
		(while (defq c (read-char _))
			(prin (char c)))
		(stream-flush (file-stream 'stdout))))

(defq usage `(
(("-h" "--help")
"Usage: cat [options] [path] ...
	options:
		-h --help: this help info.
	If no paths given on command line
	then paths are read from stdin.")
))

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;cat from stdin
			(each-line cat-file (file-stream 'stdin))
			;cat from args
			(each cat-file (slice 1 -1 args)))))
