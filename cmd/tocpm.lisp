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
(("-f" "--format") ,(opt-num 'opt_f))
))

(defun work (file)
	(when (and file
			(defq i (rfind "." file))
			(defq x (slice file i -1))
			(some (# (eql x %0)) '("cpm" "tga" "svg")))
		(defq out_file (cat (slice file 0 i) "cpm")
			canvas (canvas-load file +load_flag_noswap))
		(. canvas :save out_file opt_f)
		(print file " -> " out_file)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f 32 args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args) " -f " opt_f " " %0)) jobs)
					10000000)))))
