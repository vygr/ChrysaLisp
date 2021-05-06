(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "lib/options/options.inc")

(defun launch (node)
	(if (find (setq node (to-net-id node)) nodes)
		(open-remote "gui/gui/gui.lisp" node +kn_call_child)))

(defq usage `(
(("-h" "--help")
"Usage: gui [node ...]
	options:
		-h --help: this help info.
	Launch a GUI on nodes. If none present
	on command line will read from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(> (length (defq nodes (mail-nodes))) 0))
		(if (<= (length args) 1)
			;convert from stdin
			(each-line launch (io-stream 'stdin))
			;convert from args
			(each launch (slice 1 -1 args)))))
