(import "gui/lisp.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/streams/rle.inc")
(import "lib/streams/flm.inc")
(import "service/lock/app.inc")


(defq usage `(
(("-h" "--help")
"Usage: toflm [options] [path] ...

    options:
        -h --help: this help info.
        -f --format 1|8|12|15|16|24|32: pixel format, default 32.
        -n --name path: output film filename, default film.flm.

    Convert images to a .flm animation.

    If no paths given on command line
    then paths are read from stdin.")
(("-f" "--format") ,(opt-num 'opt_f))
(("-n" "--name") ,(opt-str 'opt_n))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f 32 opt_n "film" args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		(unless (ends-with ".flm" opt_n)
			(setq opt_n (cat opt_n ".flm")))
		(when (nempty? jobs)
			(with-write-lock opt_n
				(when (defq out_stream (file-stream opt_n +file_open_write))
					(defq film (flm-open out_stream opt_f))
					(each (lambda (file)
						(task-slice)
						(with-read-lock file
							(when (defq canvas (canvas-load file +load_flag_noswap))
								(flm-add film canvas)
								(prin file " -> " opt_n)
								(print)))) jobs)
					(flm-close film)
					(setq out_stream :nil))))))
