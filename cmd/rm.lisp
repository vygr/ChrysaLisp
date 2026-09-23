(import "lib/options/options.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: rm [options] [path] ...

    options:
        -h --help: this help info.

    If no paths given on command line
    then paths are read from stdin.")
))

;rm a file with write lock
(defun rm-file (file_path)
	(when (lock-claim-rpc file_path +lock_mode_write)
		(pii-remove file_path)
		(lock-release-rpc file_path))
	:nil)

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;rm from stdin
			(lines! rm-file (io-stream 'stdin))
			;rm from args
			(each rm-file (rest args)))))

