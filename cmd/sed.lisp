(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
{Usage: sed [options] [path] ...

	options:
		-h --help: this help info.
		-e --expression pattern: search pattern.
		-r --replace string: replacement string, default "".
		-g --global: replace all occurrences.
		-w --words: whole words.
		-x --regexp: treat pattern as regular expression.

	Stream editor. Reads from stdin if no files specified.
	Writes to stdout.})
(("-e" "--expression") ,(opt-str 'opt_e))
(("-r" "--replace") ,(opt-str 'opt_r))
(("-g" "--global") ,(opt-flag 'opt_g))
(("-x" "--regexp") ,(opt-flag 'opt_x))
(("-w" "--words") ,(opt-flag 'opt_w))
))

(defun create-rep-info (rep)
	(defq idx 0)
	(filter (# (nql %0 ""))
		(push (reduce (lambda (out ((ms me)))
			(push out (slice rep idx ms)
				(str-to-num (slice rep (inc ms) (setq idx me)))))
		(matches rep "\$\d") (list)) (slice rep idx -1))))

(defun create-reps (line match)
	(cat (reduce (lambda (out (ms me))
			(push out (slice line ms me))) match (list))
		(const (list quote (map (lambda (_) "") (str-alloc 10))))))

(defun process (stream engine pattern meta rep global rep_info)
 	(lines! (lambda (line) (task-slice)
		(print (cond
			((empty? (defq match (. engine :search line pattern meta))) line)
			((defq idx 0)
			(unless global (setq match (slice match 0 1)))
			(apply (const cat) (push (reduce (lambda (out match)
				(bind '((ms me) &ignore) match)
				(cond
					;regexp mode
					(opt_x
						(defq reps (create-reps line match))
						(reduce (lambda (out rep)
								(push out (if (num? rep) (elem-get reps rep) rep)))
							rep_info (push out (slice line idx ms))))
					;plain text mode
					((push out (slice line idx ms) rep)))
				(setq idx me) out) match (list)) (slice line idx -1)))))))
		stream))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_w :nil opt_e :nil opt_r "" opt_g :nil opt_x :nil
				args (options stdio usage))
			opt_e)
		(bind '(engine pattern meta) (query opt_e opt_w opt_x))
		(defq rep_info (if opt_x (create-rep-info opt_r)))
		(if (empty? (defq files (rest args)))
			(process (io-stream 'stdin) engine pattern meta opt_r opt_g rep_info)
			(each (# (process (file-stream %0) engine pattern meta opt_r opt_g rep_info)) files))))
