(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

;do the work on a file
(defun work (file)
	(defq defs_map (Fmap 11) uses_map (Fmap 31))
	(each-line (lambda (line) (task-slice)
			(defq line_num _
				defs (matches line "^\(de(fun|macro)\s+([^ \r\f\v\n\t\(\)]+)")
				uses (matches line "\(\s*([^ \r\f\v\n\t\(\)]+)"))
			(when (nempty? defs)
				(bind '((_ _ (x x1)) &ignore) defs)
				(. defs_map :insert (slice x x1 line) line_num))
			(when (nempty? uses)
				(each (# (bind '(_ (x x1)) %0)
					(defq use (slice x x1 line)
						line_nums (. uses_map :find use))
					(setd line_nums (list))
					(. uses_map :insert use (push line_nums line_num))) uses)))
		(file-stream file))
	(. uses_map :each (lambda (k v)
		(when (defq n (. defs_map :find k))
			(each (# (if (< %0 n) (print file " (" (inc %0)  ") " k))) v)))))

(defq usage `(
(("-h" "--help")
"Usage: forward [options] [path] ...

	options:
		-h --help: this help info.

	Scan source files for use of forward
	references to functions or macros.

	If no paths given on command line
	then will test files from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(each-line (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have do do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (cat (first args) " " %0)) jobs))))))
