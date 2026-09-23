(import "lib/options/options.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: tail [options file]

    options:
        -h --help: this help info.
        -c --count num: line count, default 10.

    Returns lines from end of file or stdin.

    Defaults to last 10 lines.")
(("-c" "--count") ,(opt-num 'opt_c))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c 10 args (options stdio usage)))
		(defq lines (list)
			collect (lambda (stream)
				(lines! (# (push lines %0)
					(if (> (length lines) opt_c) (setq lines (slice lines (- -1 opt_c) -1)))
					:nil)
					stream)))
		(if (<= (length args) 1)
			(collect (io-stream 'stdin))
			(with-read-lock (second args)
				(when (defq stream (file-stream (second args)))
					(collect stream)
					(setq stream :nil))))
		(each (const print) lines)))
