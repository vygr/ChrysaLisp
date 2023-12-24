(import "lib/options/options.inc")

;test a file
(defun test-file (file)
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
	(defq uses (list))
	(. uses_map :each (lambda (k v)
		(when (defq n (. defs_map :find k))
			(each (# (if (< %0 n) (push uses (list file (inc %0) k)))) v))))
	(each (# (print (first %0) " (" (second %0) ") " (third %0)))
		(sort (# (- (second %0) (second %1))) uses)))

(defq usage `(
(("-h" "--help")
"Usage: forward [options] [path] ...

	options:
		-h --help: this help info.

	If no paths given on command line
	then will test files from stdin.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(if (<= (length args) 1)
			;test file from stdin
			(each-line (# (test-file %0)) (io-stream 'stdin))
			;test file from args
			(each (# (test-file %0)) (rest args)))))
