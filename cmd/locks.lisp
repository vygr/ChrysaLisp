(import "lib/options/options.inc")
(import "service/lock/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: locks [options]

    options:
        -h --help: this help info.

    Print the recent lock service history.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(when (defq hist (lock-history-rpc))
			(each (const print) hist))))
