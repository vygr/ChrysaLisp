(import "lib/options/options.inc")
(import "lib/streams/diff.inc")

(defq usage `(
(("-h" "--help")
{Usage: patch [options] file_a [file_b]

	options:
		-h --help: this help info.
		-s --swap: swap sources.

	Patch text file a with text file b.
	If no second file is given it will be read from stdin.})
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
		(setd b (io-stream 'stdin))
		(when (and a b)
			(if opt_s
				(stream-patch b a (io-stream 'stdout))
				(stream-patch a b (io-stream 'stdout))))))
