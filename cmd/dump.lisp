(import "lib/options/options.inc")
(import "lib/streams/hex.inc")
(import "service/lock/app.inc")

(defun opt-toggle (opt_var)
	(static-qq (lambda (args arg)
		(setq ,opt_var (not ,opt_var)) args)))

(defq usage `(
(("-h" "--help")
"Usage: dump [options] [path] ...

    options:
        -h --help: this help info.
        -w -k --width --chunk num: chunk width, default 8.
        -o --offset: toggle byte offset column, default :t.
        -c --chars: toggle chars column, default :t.

    If no paths given on command line
    then will dump stdin.")
(("-w" "-k" "--width" "--chunk") ,(opt-num 'opt_w))
(("-o" "--offset") ,(opt-toggle 'opt_o))
(("-c" "--chars") ,(opt-toggle 'opt_c))
))

;dump a stream to stdout
(defun dump-file (stream width flags)
	(when stream
		(hex-encode-stream stream (io-stream 'stdout) width flags)))

;dump a file with read lock
(defun dump-path (file_path width flags)
	(with-read-lock file_path
		(when (defq stream (file-stream file_path))
			(dump-file stream width flags)
			(setq stream :nil))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_w 8 opt_o :t opt_c :t
				args (options stdio usage)))
		(defq flags (+ (if opt_o +hex_stream_flag_offset 0)
			(if opt_c +hex_stream_flag_chars 0)))
		(if (<= (length args) 1)
			;dump from stdin
			(dump-file (io-stream 'stdin) opt_w flags)
			;dump from args as files
			(each (# (dump-path %0 opt_w flags)) (rest args)))))

