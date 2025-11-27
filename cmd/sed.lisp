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

(defun sed-replace (line match rep global)
	(prin match)(print)
	(defq idx 0)
	(unless global (setq match (list (first match))))
	(apply (const cat) (push (reduce (lambda (out ((ms me) &ignore))
			(push out (slice line idx ms) rep)
			(setq idx me) out)
		match (list)) (slice line idx -1))))

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
		(if (empty? (defq files (rest args)))
			(process (io-stream 'stdin) engine pattern meta opt_r opt_g)
			(each (# (process (file-stream %0) engine pattern meta opt_r opt_g)) files))))
