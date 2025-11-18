(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: nl [options] [file ...]

	options:
		-h --help: this help info.
		-b --body-numbering mode: numbering mode (a=all, t=non-empty, n=none), default 't'.
		-s --number-separator str: separator between number and line, default TAB.
		-v --starting-line-number num: starting line number, default 1.
		-w --number-width num: width of line number field, default 6.

	Number lines of files or stdin.")
(("-b" "--body-numbering") ,(opt-str 'opt_mode))
(("-s" "--number-separator") ,(opt-str 'opt_sep))
(("-v" "--starting-line-number") ,(opt-num 'opt_start))
(("-w" "--number-width") ,(opt-num 'opt_width))
))

(defun number-lines (stream)
	(defq line_num opt_start)
	(lines! (lambda (line)
			(cond
				;all lines
				((eql opt_mode "a")
					(print (pad (str line_num) opt_width " ") opt_sep line)
					(setq line_num (inc line_num)))
				;non-empty lines only
				((eql opt_mode "t")
					(if (> (length line) 0)
						(progn
							(print (pad (str line_num) opt_width " ") opt_sep line)
							(setq line_num (inc line_num)))
						(print line)))
				;no numbering
				:t
					(print line)))
		stream))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_mode "t" opt_sep (ascii-char 9) opt_start 1 opt_width 6 args (options stdio usage)))
		(if (<= (length args) 1)
			;read from stdin
			(number-lines (io-stream 'stdin))
			;read from files
			(each (# (when (defq stream (file-stream %0))
					(number-lines stream)))
				(rest args)))))
