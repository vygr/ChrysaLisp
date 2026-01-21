(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

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

;do the work on a file
(defun work (file)
	(defq defs_map (Fmap 11) uses_map (Fmap 101))
	(files-scan file (lambda (input file line idx)
		(defq defs (matches input "^\(def(un|macro)\s+([^ \r\f\v\n\t()]+)")
			uses (matches input "\(\s*(\D[^ \r\f\v\n\t()]*)"))
		(when (nempty? defs)
			(bind '((x x (x x1)) &ignore) defs)
			(. defs_map :insert (slice input x x1) idx))
		(when (nempty? uses)
			(each (# (bind '(x (x x1)) %0)
				(. uses_map :update (slice input x x1)
					(# (if %0 (push %0 idx) (list idx))))) uses)) :nil))
	(. uses_map :each (lambda (k v)
		(when (defq n (. defs_map :find k))
			(each (# (if (< %0 n) (print file " (" (inc %0)  ") " k))) v)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) 1)
			;have to do the work when just 1 file !
			(work (pop jobs))
			;do them all out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (cat (first args) " " %0)) jobs))))))
