(import "lib/options/options.inc")

(defun opt-listen (opt_var)
	(static-qq (lambda (args arg)
		(if (and (nempty? args) (some (# (find %0 "0123456789")) (first args)) (not (starts-with "-" (first args))))
			(progn (setq ,opt_var (str-as-num (first args))) (rest args))
			(progn (setq ,opt_var 3333) args)))))

(defq usage `(
(("-h" "--help")
"Usage: link [options] [name | host[:port] ...]

    options:
        -h --help: this help info.
        -l --listen [port]: listen for incoming TCP network link (default: 3333).
        -v --verbose: verbose output.

    Start SHMEM or TCP network link driver/s.

    Network links:
        link -l 3333          ; Listen on port 3333 for incoming network link
        link 192.168.1.100    ; Connect to peer on default port 3333
        link 127.0.0.1:3333   ; Connect to peer on specified port

    SHMEM links:
        `CLB-L1 CLB-L2`, are the names of the ChrysaLib `-shm` links.
        Internal Lisp subnet links are of the form `000-000`, `001-002`, etc.

    If no links names given on command line and -l not passed,
    then names are read from stdin.")
(("-l" "--listen") ,(opt-listen 'opt_l))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun start-link (target verbose)
	(if (or (find ":" target) (find "." target))
		(progn
			(when verbose (print "Starting network link: " target))
			(mail-send (open-child "service/net/link" +kn_call_open) target))
		(progn
			(when verbose (print "Starting SHMEM link: " target))
			(mail-send (open-child "sys/link/link" +kn_call_child) target))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_l :nil opt_v :nil args (options stdio usage)))
		(when opt_l
			(start-link (cat ":" (str opt_l)) opt_v))
		(if (<= (length args) 1)
			(unless opt_l
				;from stdin
				(lines! (# (start-link %0 opt_v) :nil) (io-stream 'stdin)))
			;from args
			(each (# (start-link %0 opt_v)) (rest args)))))

