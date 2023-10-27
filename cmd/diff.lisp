(import "class/lisp.inc")
(import "sys/lisp.inc")
(import "lib/options/options.inc")
(import "lib/text/diff.inc")

(defq usage `(
(("-h" "--help")
{Usage: diff [options] file_a [file_b]

	options:
		-h --help: this help info.
		-r --reverse: switch sources.

	Calculate difference between text file a and text file b.
	If no second file is given it will be read from stdin.
})
(("-r" "--reverse")
	,(lambda (args arg) (setq r :t) args))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq r :nil args (options stdio usage))
			(<= 2 (length args) 3))
		(bind '(a &optional b) (map file-stream (rest args)))
		(unless b (setq b (io-stream 'stdin)))
		(when (and a b)
			(if r (defq c a a b b c c :nil))
			(stream-diff a b (io-stream 'stdout)))))
