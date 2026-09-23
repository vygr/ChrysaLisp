(import "lib/options/options.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: save [options] [path] ...

    options:
        -h --help: this help info.
        -s --stdout: pass through, default :nil.

    Read from stdin, write to all given paths,
    optionally write to stdout.")
(("-s" "--stdout") ,(opt-flag 'opt_s))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s :nil args (options stdio usage)))
		(defq stdin (io-stream 'stdin) stdout (io-stream 'stdout)
			paths (rest args)
			locks (filter (# (lock-claim-rpc %0 +lock_mode_write)) paths)
			files (filter (# %0) (map (# (file-stream %0 +file_open_write)) locks)))
		(while (defq c (read-blk stdin 1024))
			(if opt_s (write-blk stdout c))
			(each (# (write-blk %0 c)) files))
		(each stream-flush files)
		(if opt_s (stream-flush stdout))
		(setq files :nil)
		(each lock-release-rpc locks)))


