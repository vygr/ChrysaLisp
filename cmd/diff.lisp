(import "class/lisp.inc")
(import "sys/lisp.inc")
(import "lib/options/options.inc")
(import "lib/text/diff.inc")

(defq usage `(
(("-h" "--help")
{Usage: diff [options] file_a [file_b]

	options:
		-h --help: this help info.
		-s --swap: swap sources.

	Calculate difference between text file a and text file b.
	If no second file is given it will be read from stdin.
})
(("-s" "--swap")
	,(lambda (args arg) (setq opt_s :t) args))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s :nil args (options stdio usage))
			(<= 2 (length args) 3))
		(bind '(a &optional b) (map file-stream (rest args)))
		(unless b (setq b (io-stream 'stdin)))
		(when (and a b)
			(if opt_s (defq c a a b b c c :nil))
			(stream-diff a b (io-stream 'stdout)))))
