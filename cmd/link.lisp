(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: link [options] CLB-L1 CLB-L2 000-000 ...

	options:
		-h --help: this help info.

	Start SHMEM link driver/s.

	`CLB-L1 CLB-L2`, are the names of the ChrysaLib `-shm` links.
	If you're bridging Lisp subnets over CLB.

	Internal Lisp subnet links are of the form `001-002`, or if
	connecting local Lisp subnets the recommended form is
	`000-000` for subnet bridge 1, `001-001` for subnet bridge 2 etc.

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
			(lines! (# (mail-send (open-child "sys/link/link" +kn_call_child) %0)) (io-stream 'stdin))
			;from args
			(each (# (mail-send (open-child "sys/link/link" +kn_call_child) %0)) (rest args)))))
