(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: sleep duration

	options:
		-h --help: this help info.

	Pause for DURATION seconds.
	DURATION can be an integer or floating point number.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			(print "sleep: missing operand")
			(progn
				(defq duration (real (second args)))
				;convert seconds to microseconds (task-sleep takes microseconds)
				(defq usec (* duration 1000000))
				(task-sleep usec)))))
