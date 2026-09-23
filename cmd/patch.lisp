(import "lib/options/options.inc")
(import "lib/streams/diff.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: patch [options] file_a [file_b]

    options:
        -h --help: this help info.
        -s --swap: swap sources.

    Patch text file a with text file b.

    If no second file is given it will
    be read from stdin.")
(("-s" "--swap") ,(opt-flag 'opt_s))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s :nil args (options stdio usage))
			(<= 2 (length args) 3))
		(bind '(file_a &optional file_b) (rest args))
		(when (lock-claim-rpc file_a +lock_mode_read)
			(if file_b
				(when (lock-claim-rpc file_b +lock_mode_read)
					(when (and (defq a (file-stream file_a)) (defq b (file-stream file_b)))
						(if opt_s
							(stream-patch b a (io-stream 'stdout))
							(stream-patch a b (io-stream 'stdout))))
					(setq a :nil b :nil)
					(lock-release-rpc file_b))
				(progn
					(when (and (defq a (file-stream file_a)) (defq b (io-stream 'stdin)))
						(if opt_s
							(stream-patch b a (io-stream 'stdout))
							(stream-patch a b (io-stream 'stdout))))
					(setq a :nil)))
			(lock-release-rpc file_a))))

