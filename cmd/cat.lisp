(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: cat [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then paths are read from stdin.")
))

;cat a file to stdout
(defun cat-file (_)
	(if (setq _ (file-stream _))
		(while (defq c (read-char _))
			(prin (char c)))
		(stream-flush (io-stream 'stdout))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;cat from stdin
			(each-line cat-file (io-stream 'stdin))
			;cat from args
			(each cat-file (rest args)))))
