(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: slice [options]

	options:
		-h --help: this help info.
		-s --start num: start char index, default 0.
		-e --end num: end char index, default -1.

	Slice the lines from stdin to stdout.")
(("-s" "--start")
	,(lambda (args arg)
		(setq opt_s (str-as-num (first args)))
		(rest args)))
(("-e" "--end")
	,(lambda (args arg)
		(setq opt_e (str-as-num (first args)))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s 0 opt_e -1 args (options stdio usage)))
		;slice stdin
		(each-line (# (print (slice %0 opt_s opt_e))) (io-stream 'stdin))))
