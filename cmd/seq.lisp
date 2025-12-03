(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: seq [options] [first [incr]] last

	options:
		-h --help: this help info.
		-s --separator str: separator string, default newline.
		-w --width: equalize width by padding with leading zeros.

	Print numbers from FIRST to LAST, in steps of INCR.
	FIRST and INCR default to 1.")
(("-s" "--separator") ,(opt-str 'opt_sep))
(("-w" "--width") ,(opt-flag 'opt_width))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_sep (ascii-char 10) opt_width :nil args (options stdio usage)))
		(cond
			;seq last
			((<= (length args) 2)
				(setq first 1 incr 1 last (num (second args))))
			;seq first last
			((<= (length args) 3)
				(setq first (num (second args)) incr 1 last (num (elem 2 args))))
			;seq first incr last
			:t
				(setq first (num (second args)) incr (num (elem 2 args)) last (num (elem 3 args))))
		;determine field width if needed
		(when opt_width
			(defq width (max (length (str first)) (length (str last)))))
		;generate sequence
		(defq n first)
		(cond
			;ascending
			((>= incr 0)
				(while (<= n last)
					(if opt_width
						(print (pad (str n) width "0"))
						(print n))
					(setq n (+ n incr))))
			;descending
			:t
				(while (>= n last)
					(if opt_width
						(print (pad (str n) width "0"))
						(print n))
					(setq n (+ n incr))))))
