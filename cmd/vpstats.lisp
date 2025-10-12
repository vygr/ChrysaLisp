(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: vpstats [options] [path] ...

	options:
		-h --help: this help info.

	Scan for VP instruction usage stats.

	If no paths given on command line
	then will take paths from stdin.")
))

;do the work on a file
(defun work (file)
	(lines! (lambda (line)
		(defq split_line (split line (const (char-class " ()'\t\r\q")))
			type (sym (first split_line)))
		(when (starts-with "emit-" type)
			(if (defq i (some (# (if (eql (first %0) type) (!))) inst_list))
				(elem-set (elem-get inst_list i) 1 (inc (second (elem-get inst_list i))))
				(push inst_list (list type 1)))))
		(file-stream file)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq inst_list (list))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		;gather length stats
		(each (const work) (usort jobs))
		;display results
		(print "VP instruction usage stats")
		(each (# (print "inst: " (pad (first %0) 16) " cnt: " (pad (second %0) 5)))
			(sort inst_list (# (- (second %1) (second %0)))))))
