(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: true

	options:
		-h --help: this help info.

	Exit with a status code indicating success (0).")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;do nothing, successful exit
		:t))
