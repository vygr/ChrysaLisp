(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: false

	options:
		-h --help: this help info.

	Exit with a status code indicating failure.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;return nil to indicate failure
		:nil))
