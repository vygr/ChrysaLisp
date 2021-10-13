(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: split [options]
	options:
		-h --help: this help info.
		-s --sep separator: default ,.
		-e --sel num: selected element, default nil.
	Split the lines from stdin to stdout.
	Optionaly select a specific element of
	the split.")
(("-s" "--sep")
	,(lambda (args arg)
		(setq sep (elem 0 args))
		(slice 1 -1 args)))
(("-e" "--elem")
	,(lambda (args arg)
		(setq sel (str-as-num (elem 0 args)))
		(slice 1 -1 args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq sep "," sel nil args (options stdio usage)))
		;split stdin
		(each-line (#
			(defq elms (split %0 sep))
			(if (and sel (> (length elms) sel))
				(setq elms (list (elem sel elms))))
			(each print elms)) (io-stream 'stdin))))
