(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/files/files.inc")
(import "service/lock/app.inc")

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

;do the work on a file
(defun work (file)
	(with-read-lock file
		;the imports of the file, (files-scan), each with the line it is on,
		;in the order of the lines
		(defq changed :nil out_stream (if opt_w (memory-stream))
			imps (filter (# (eql (first %0) "import")) (files-scan file)) at 0)
		(when (and (nempty? imps) (defq in_stream (file-stream file)))
			(lines! (lambda (line)
					(when (and (< at (length imps)) (= (inc (!)) (second (defq imp (elem-get imps at)))))
						;a second on the line is left as it is
						(while (and (< (++ at) (length imps)) (= (second (elem-get imps at)) (second imp))))
						(defq old_path (third imp)
							new_path (path-to-relative (path-to-absolute old_path file) file)
							q1 (inc (bskipn (const (char-class "\q{")) line 0)) q2 (+ q1 (length old_path)))
						(when (and (nql old_path new_path) (eql old_path (slice line q1 q2)))
							(unless changed
								(print "File: " file)
								(setq changed :t))
							(defq new_line (cat (slice line 0 q1) new_path (slice line q2 -1)))
							(print "- " line)
							(print "+ " new_line)
							(setq line new_line)))
					(if opt_w (write-line out_stream line))
					:nil)
				in_stream)
			(setq in_stream :nil)))
	;overwrite the file if we actually performed any optimizations
	(and changed opt_w
		(with-write-lock file
			(stream-seek out_stream 0 0)
			(when (defq dest (file-stream file +file_open_write))
				(while (defq c (read-blk out_stream 1024))
					(write-blk dest c))
				(stream-flush dest)
				(setq dest :nil)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j 8 opt_w :nil args (options stdio usage)))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0) :nil) (io-stream 'stdin)))
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
