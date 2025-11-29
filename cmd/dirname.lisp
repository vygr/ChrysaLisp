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
				;remove trailing slashes
				(while (and (> (length path) 1) (eql (elem -2 path) "/"))
					(setq path (slice 0 -1 path)))
				;find last slash
				(defq dir (if (defq pos (find-rev "/" path))
					(progn
						;extract directory part
						(setq dir (slice 0 pos path))
						;remove trailing slashes from result
						(while (and (> (length dir) 1) (eql (elem -2 dir) "/"))
							(setq dir (slice 0 -1 dir)))
						(if (eql dir "") "/" dir))
					"."))
				(print dir)))))
