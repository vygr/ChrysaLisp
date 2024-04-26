(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: head [options file]

	options:
		-h --help: this help info.
		-c --count num: line count, default 10.

	Returns lines from start of file or stdin.

	Defaults to first 10 lines.")
(("-c" "--count")
	,(lambda (args arg)
		(setq opt_c (str-as-num (first args)))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c 10 args (options stdio usage)))
		(defq lines (list))
		(each-line (# (if (< (length lines) opt_c) (push lines %0)))
			(if (<= (length args) 1) (io-stream 'stdin) (file-stream (second args))))
		(each (const print) lines)))
