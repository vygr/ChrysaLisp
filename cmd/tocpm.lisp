(import "gui/lisp.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: tocpm [options] [path] ...

	options:
		-h --help: this help info.
		-f --format 1|8|12|15|16|24|32: pixel format, default 32.

	Load the images and save as .cpm images.

	If no paths given on command line
	then paths are read from stdin.")
(("-f" "--format")
	,(lambda (args arg)
		(setq format (str-as-num (first args)))
		(rest args)))
))

(defun work (file)
	(when (and file (defq i (find-rev "." file)))
		(defq out_file (cat (slice 0 i file) ".cpm")
			canvas (Canvas-from-file file +load_flag_noswap))
		(. canvas :save out_file format)
		(print file " -> " out_file)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq format 32 args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(each-line (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have do do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args) " -f " format " " %0)) jobs)
					10000000)))))
