(import "class/lisp.inc")
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
		(setq start (str-as-num (elem-get 0 args)))
		(slice 1 -1 args)))
(("-e" "--end")
	,(lambda (args arg)
		(setq end (str-as-num (elem-get 0 args)))
		(slice 1 -1 args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq start 0 end -1 args (options stdio usage)))
		;slice stdin
		(each-line (# (print (slice start end %0))) (io-stream 'stdin))))
