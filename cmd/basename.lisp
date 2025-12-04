(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: basename path [suffix]

	options:
		-h --help: this help info.

	Print NAME with any leading directory components removed.
	If specified, also remove a trailing SUFFIX.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			(print "basename: missing operand")
			(progn
				(defq path (second args))
				(defq suffix (if (> (length args) 2) (elem-get args 2) ""))
				;remove trailing slashes (-2 is last element, slice to -2 removes it)
				(while (and (> (length path) 1) (eql (elem-get path -2) "/"))
					(setq path (slice path 0 -2)))
				;extract basename (rfind returns pos after match, -1 is end boundary)
				(defq name (if (defq pos (rfind "/" path))
					(slice path pos -1)
					path))
				;remove suffix if specified and present
				(when (and (> (length suffix) 0)
						(ends-with suffix name))
					(setq name (slice name 0 (- -1 (length suffix)))))
				(print name)))))
