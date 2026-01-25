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

(defun process (stream engine find_meta rep_meta global)
 	(lines! (lambda (line) (task-slice)
		(print (cond
			((empty? (defq match (. engine :search line find_meta))) line)
			(:t (unless global (setq match (slice match 0 1)))
				(replace-matches line match rep_meta)))))
		stream))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_w :nil opt_e :nil opt_r "" opt_g :nil opt_x :nil
				args (options stdio usage))
			opt_e)
		(bind '(engine find_meta &ignore) (query opt_e opt_w opt_x))
		(defq rep_meta (replace-compile opt_r))
		(if (empty? (defq files (rest args)))
			(process (io-stream 'stdin) engine find_meta rep_meta opt_g)
			(each (# (process (file-stream %0) engine find_meta rep_meta opt_g)) files))))
