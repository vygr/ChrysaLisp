(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: rev [file ...]

	options:
		-h --help: this help info.

	Reverse the characters in each line of input.
	If no files are given, reads from stdin.")
))

(defun rev-line (line)
	(print (reverse line)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;read from stdin
			(lines! rev-line (io-stream 'stdin))
			;read from files
			(each (# (when (defq stream (file-stream %0))
					(lines! rev-line stream)))
				(rest args)))))
