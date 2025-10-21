(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: split [options]

	options:
		-h --help: this help info.
		-s --sep separator: default ,.
		-e --sel num: selected element, default :nil.

	Split the lines from stdin to stdout.

	Optionally select a specific element of
	the split.")
(("-s" "--sep") ,(opt-num 'opt_s))
(("-e" "--elem") ,(opt-num 'opt_e))
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
