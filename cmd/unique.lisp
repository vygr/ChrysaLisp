;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: unique [options] [line] ...
	options:
		-h --help: this help info.
	If no lines given on command line
	then will read lines from stdin.")
))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq args (options slave usage)))
	(defq stdin (file-stream 'stdin))
	(if (<= (length args) 1)
		;from stdin
		(when (defq ll (read-line stdin))
			(print ll)
			(while (defq nl (read-line stdin))
				(unless (eql ll nl)
					(print (setq ll nl)))))
		;from args
		(progn
			(print (defq ll (elem 1 args)))
			(each (lambda (nl)
				(unless (eql ll nl)
					(print (setq ll nl)))) (slice 2 -1 args)))))
