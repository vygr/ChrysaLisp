(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: dirname path

	options:
		-h --help: this help info.

	Print directory portion of path.
	If path contains no slashes, output '.' (current directory).")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			(print "dirname: missing operand")
			(progn
				(defq path (second args))
				;remove trailing slashes (-2 is last element)
				(while (and (> (length path) 1) (eql (elem-get path -2) "/"))
					(setq path (slice path 0 -2)))
				;find last slash (rfind returns pos after match)
				(defq dir (if (defq pos (rfind "/" path))
					(progn
						;extract directory part (dec pos to exclude the slash)
						(defq d (slice path 0 (dec pos)))
						;remove trailing slashes from result
						(while (and (> (length d) 1) (eql (elem-get d -2) "/"))
							(setq d (slice d 0 -2)))
						(if (eql d "") "/" d))
					"."))
				(print dir)))))
