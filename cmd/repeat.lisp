(import "lib/options/options.inc")
(import "lib/task/pipe.inc")

(defq usage `(
(("-h" "--help")
"Usage: repeat [options] command_line

	options:
		-h --help: this help info.
		-c --count: count, defult 10.

	Repeat run command line.")
(("-c" "--count")
	,(lambda (args arg)
		(setq opt_c (str-as-num (first args)))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c 10 args (options stdio usage)))
		(defq pipe (join (rest args) " "))
		(times opt_c
			(pipe-run pipe prin)
			(stream-flush (io-stream 'stdout)))))
