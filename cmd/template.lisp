(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: template [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.

	Template command app for you to copy as
	a starting point.

	Add your description here.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
))

;do the work on a file
(defun work (file)
	(print "Work on file: " file))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
