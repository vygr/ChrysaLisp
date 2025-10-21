(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: cat [options] [path] ...

	options:
		-h --help: this help info.
		-f --file: prepend file name.

	If no paths given on command line
	then paths are read from stdin.")
(("-f" "--file") ,(opt-flag 'opt_f))
))

;cat a file to stdout
(defun cat-file (file)
	(when (defq stream (file-stream file))
		(when opt_f
			(print (defq banner (pad "" (+ (length file) 2) ";;;;;;;;")))
			(print "; " file)
			(print banner))
		(while (defq c (read-char stream))
			(prin (char c)))
		(stream-flush (io-stream 'stdout))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f :nil args (options stdio usage)))
		(if (<= (length args) 1)
			;cat from stdin
			(lines! cat-file (io-stream 'stdin))
			;cat from args
			(each cat-file (rest args)))))
