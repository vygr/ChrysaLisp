(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: link [options] 000-000 ...
	options:
		-h --help: this help info.
	Start SHMEM link driver/s.
	If no links names given on command line
	then names are read from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;start shmem links !
		(if (<= (length args) 1)
			;from stdin
			(each-line (# (mail-send (open-child "sys/link/link" +kn_call_child) %0)) (io-stream 'stdin))
			;from args
			(each (# (mail-send (open-child "sys/link/link" +kn_call_child) %0)) (slice 1 -1 args)))))
