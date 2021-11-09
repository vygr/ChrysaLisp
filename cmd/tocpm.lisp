(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/options/options.inc")

(defun conv-file (in_file)
	(unless (eql in_file "")
		(defq out_file (cat (slice 0 (find-rev "." in_file) in_file) ".cpm")
			canvas (Canvas-from-file in_file +load_flag_noswap))
		(. canvas :save out_file format)
		(print in_file " -> " out_file)
		(stream-flush (io-stream 'stdout))))

(defq usage `(
(("-h" "--help")
"Usage: tocmp [options] [path] ...
	options:
		-h --help: this help info.
		-f --format 1|8|12|15|16|24|32: pixel format, default 32.
	Load the images and save as .cpm images.
	If no paths given on command line
	then paths are read from stdin.")
(("-f" "--format")
	,(lambda (args arg)
		(setq format (str-as-num (elem-get 0 args)))
		(slice 1 -1 args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq format 32 args (options stdio usage)))
		(if (<= (length args) 1)
			;convert from stdin
			(each-line conv-file (io-stream 'stdin))
			;convert from args
			(each conv-file (slice 1 -1 args)))))
