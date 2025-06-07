(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: split [options]

	options:
		-h --help: this help info.
		-s --sep separator: default ,.
		-e --sel num: selected element, default :nil.

	Split the lines from stdin to stdout.

	Optionaly select a specific element of
	the split.")
(("-s" "--sep")
	,(lambda (args arg)
		(setq opt_s (first args))
		(rest args)))
(("-e" "--elem")
	,(lambda (args arg)
		(setq opt_e (str-as-num (first args)))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s "," opt_e :nil args (options stdio usage)))
		;split stdin
		(lines! (#
			(defq elms (split %0 (char-class opt_s)))
			(if (and opt_e (> (length elms) opt_e))
				(setq elms (list (elem-get elms opt_e))))
			(each (const print) elms)) (io-stream 'stdin))))
