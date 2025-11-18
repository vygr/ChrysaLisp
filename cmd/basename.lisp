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
				(defq suffix (if (> (length args) 2) (elem 2 args) ""))
				;remove trailing slashes
				(while (and (> (length path) 1) (eql (elem -2 path) "/"))
					(setq path (slice 0 -1 path)))
				;extract basename
				(defq name (if (defq pos (find-rev "/" path))
					(slice (inc pos) -1 path)
					path))
				;remove suffix if specified and present
				(when (and (> (length suffix) 0)
						(ends-with suffix name))
					(setq name (slice 0 (- (length suffix)) name)))
				(print name)))))
