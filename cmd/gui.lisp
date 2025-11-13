(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: gui [node ...]

	options:
		-h --help: this help info.

	Launch a GUI on nodes.

	If none present on command line then
	will read from stdin.")
))

(defun launch (node)
	(if (find (setq node (hex-decode node)) nodes)
		(open-remote "service/gui/app.lisp" node +kn_call_child)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(> (length (defq nodes (map cat (lisp-nodes)))) 0))
		(if (<= (length args) 1)
			;convert from stdin
			(lines! launch (io-stream 'stdin))
			;convert from args
			(each launch (rest args)))))
