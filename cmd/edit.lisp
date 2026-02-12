(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/text/document.inc")

(defq usage `(
(("-h" "--help")
"Usage: edit [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.
		-t --trim: trim leading/trailing whitespace and empty lines.

	Command line text editor.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-t" "--trim") ,(opt-flag 'opt_t))
))

;do the work on a file
(defun work (file opt_t)
	(defq doc (Document))
	(catch
		(progn
			(. doc :stream_load (file-stream file))
			(if opt_t (. doc :trim))
			(. doc :stream_save (file-stream file +file_open_write))
			(print "Edited: " file))
		(print "Error editing " file ": " _)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 1 opt_t :nil args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (# (work %0 opt_t)) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_t " -t" "")
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))
