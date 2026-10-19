(import "gui/lisp.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/streams/rle.inc")
(import "lib/media/flm.inc")
(import "service/lock/app.inc")


(defq usage `(
(("-h" "--help")
"Usage: toflm [options] [path] ...

    options:
        -h --help: this help info.
        -f --format 1|8|12|15|16|24|32: pixel format, default 32.
        -n --name path: output film filename, default film.flm.
        -r --rate fps: frames a second, default 30.
        -c --convert: the films named, of an older version, made
            this version in place: each a packet a frame, the rate
            in the header.

    Convert images to a .flm animation.

    If no paths given on command line
    then paths are read from stdin.")
(("-f" "--format") ,(opt-num 'opt_f))
(("-n" "--name") ,(opt-str 'opt_n))
(("-r" "--rate") ,(opt-num 'opt_r))
(("-c" "--convert") ,(opt-flag 'opt_c))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f 32 opt_n "film" opt_r 30 opt_c :nil args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		(when opt_c
			;each film read whole, so it can be written over
			(each (lambda (file)
				(with-write-lock file
					(defq out (memory-stream) frames (and (defq text (load file))
						(flm-convert (string-stream text) out opt_r)))
					(cond
						(frames (stream-seek out 0 0)
							(defq parts (list) part :nil)
							(while (nempty? (setq part (ifn (read-blk out 1048576) ""))) (push parts part))
							(save (apply (const cat) (cat (list "") parts)) file)
							(print file ": " frames " frames, version 3"))
						((print file ": not an older film, left as it is")))))
				jobs)
			(setq jobs (list)))
		(unless (ends-with ".flm" opt_n)
			(setq opt_n (cat opt_n ".flm")))
		(when (nempty? jobs)
			(with-write-lock opt_n
				(when (defq out_stream (file-stream opt_n +file_open_write))
					(defq film (flm-open out_stream opt_f opt_r))
					(each (lambda (file)
						(task-slice)
						(with-read-lock file
							(when (defq canvas (canvas-load file +load_flag_noswap))
								(flm-add film canvas)
								(prin file " -> " opt_n)
								(print)))) jobs)
					(flm-close film)
					(setq out_stream :nil))))))
