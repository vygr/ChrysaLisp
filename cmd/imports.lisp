(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: imports [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 8.
		-w --write: write new file, default :nil.

	Scan for import statements in .vp, .inc, .lisp files,
	replacing any import lines with an optimal relative or
	absolute file path.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-w" "--write") ,(opt-flag 'opt_w))
))

(defq +split_class (char-class " ()'\t\r\q{@}<>"))

;do the work on a file
(defun work (file)
	(defq changed :nil out_stream (if opt_w (memory-stream)))
	(lines! (lambda (line)
		;short circuit non-import lines as they must start with (
		(when (and (> (length line) 10) (eql (first line) "("))
			(defq s (split line +split_class))
			(when (and (> (length s) 1) (eql (first s) "import") (defq q1 (find "\q" line)))
				(defq q2 (find "\q" line (inc q1))
					old_path (slice line (inc q1) q2)
					new_path (path-to-relative (path-to-absolute old_path file) file))
				;ensure we are looking at the string token before mutating
				(when (and (eql old_path (second s)) (nql old_path new_path))
					(unless changed
						(print "File: " file)
						(setq changed :t))
					(defq new_line (cat (slice line 0 (inc q1)) new_path (slice line q2 -1)))
					(print "- " line)
					(print "+ " new_line)
					(setq line new_line))))
		(if opt_w (write-line out_stream line)))
		(file-stream file))
	;overwrite the file if we actually performed any optimizations
	(when (and changed opt_w)
		(stream-seek out_stream 0 0)
		(defq dest (file-stream file +file_open_write))
		(while (defq c (read-blk out_stream 1024))
			(write-blk dest c))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		;filter to only the requested script extensions
		(setq jobs (filter (lambda (f) (some (# (ends-with %0 f)) '(".vp" ".inc" ".lisp"))) jobs))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (lambda ((job result)) (prin result))
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						(if opt_w " -w " " ")
						(slice (str %0) 1 -2)))
					(partition jobs opt_j)))))))