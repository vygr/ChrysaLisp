(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: template [options] [path] ...

	options:
		-h --help: this help info.

	Template command app for you to copy as
	a starting point.

	Add your description here.

	If no paths given on command line
	then will take paths from stdin.")
))

;do the work on a file
(defun work (file)
	(print "Work on file: " file))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(each-line (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (cat (first args) " " %0)) jobs))))))
