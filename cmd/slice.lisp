(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: slice [options]

	options:
		-h --help: this help info.
		-s --start num: start char index, default 0.
		-e --end num: end char index, default -1.

	Slice the lines from stdin to stdout.")
(("-s" "--start") ,(opt-num 'opt_s))
(("-e" "--end") ,(opt-num 'opt_e))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s 0 opt_e -1 args (options stdio usage)))
		;slice stdin
		(lines! (lambda (line)
				(defq len (length line)
					s (if (< opt_s 0) (+ len 1 opt_s) opt_s)
					e (if (< opt_e 0) (+ len 1 opt_e) opt_e))
				(print (slice line (max 0 (min s len)) (max 0 (min e len)))))
			(io-stream 'stdin))))
