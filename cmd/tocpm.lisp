(import "gui/lisp.inc")
(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/image/cpm.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: tocpm [options] [path] ...

    options:
        -h --help: this help info.
        -f --format 1|8|12|15|16|24|32: pixel format, default 32.
        -r --rle: enable run-length encoding, default :nil.
        -l --lz4: enable lz4 compression, default :nil.

    Load the images and save as .cpm images.

    If no paths given on command line
    then paths are read from stdin.")
(("-f" "--format") ,(opt-num 'opt_f))
(("-r" "--rle") ,(opt-flag 'opt_r))
(("-l" "--lz4") ,(opt-flag 'opt_l))
))

(defun work (file)
	(when (and file (defq i (rfind "." file)))
		(defq out_file (cat (slice file 0 i) "cpm"))
		(when (lock-claim-rpc file +lock_mode_read)
			(when (lock-claim-rpc out_file +lock_mode_write)
				(when (defq canvas (canvas-load file +load_flag_noswap))
					(when (defq out_stream (file-stream out_file +file_open_write))
						(CPM-save canvas out_stream opt_f opt_r opt_l)
						(stream-flush out_stream)
						(setq out_stream :nil))
					(prin file " -> " out_file)
					(print))
				(lock-release-rpc out_file))
			(lock-release-rpc file))))


(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_f 32 opt_r :nil opt_l :nil args (options stdio usage)))
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(if (nempty? jobs) (work (pop jobs)))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args) " -f " opt_f
								(if opt_r " -r" "")
								(if opt_l " -l" "")
								" " %0)) jobs)
					10000000)))))
