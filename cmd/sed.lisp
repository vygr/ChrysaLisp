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
	(push (reduce (lambda (out ((ms me)))
			(push out (list (slice rep idx ms)
				(str-to-num (slice rep (inc ms) me))))
			(setq idx me) out)
		(matches rep "\$\d") (list)) (list (slice rep idx -1) :nil)))

(defun create-reps (line match)
	(defq out (list "" "" "" "" "" "" "" "" "" ""))
	(each (lambda ((ms me)) (elem-set out (!) (slice line ms me))) match)
	out)

(defun sed-replace (line match rep global)
	(defq idx 0)
	(unless global (setq match (list (first match))))
	(apply (const cat) (push (reduce (lambda (out match)
		(bind '((ms me) &ignore) match)
		(push out (slice line idx ms))
		(cond
			(opt_x ;regexp mode
				(defq reps (create-reps line match))
				(each (lambda ((rep idx))
					(push out rep (if idx (elem-get reps idx) ""))) rep_info))
			(:t	;plain text mode
				(push out rep)))
		(setq idx me) out) match (list)) (slice line idx -1))))

(defun process (stream engine pattern meta rep global)
	(lines! (lambda (line)
		(defq match (. engine :search line pattern meta))
		(if (empty? match)
			(print line)
			(print (sed-replace line match rep global))))
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
			(process (io-stream 'stdin) engine pattern meta opt_r opt_g)
			(each (# (process (file-stream %0) engine pattern meta opt_r opt_g)) files))))
