(import "lib/options/options.inc")
(import "lib/task/cmd.inc")

(defq usage `(
(("-h" "--help")
"Usage: grep [options] [pattern] [path] ...

	options:
		-h --help: this help info.
		-e --exp pattern: regular expression.
		-f --file: file mode, default :nil.
		-w --words: whole words mode, default :nil.
		-r --regexp: regexp mode, default :nil.
		-c --coded: encoded pattern mode, default :nil.
		-m --md: md doc mode, default :nil.

	pattern:
		^  start of line
		$  end of line
		!  start/end of word
		.  any char
		+  one or more
		*  zero or more
		?  zero or one
		|  or
		[] class, [0-9], [abc123]
		() group
		\r return
		\f form feed
		\v vertical tab
		\n line feed
		\t tab
		\s [ \t]
		\S [^ \r\f\v\n\t]
		\d [0-9]
		\D [^0-9]
		\l [a-z]
		\u [A-Z]
		\a [A-Za-z]
		\p [A-Za-z0-9]
		\w [A-Za-z0-9_]
		\W [^A-Za-z0-9_]
		\x [A-Fa-f0-9]
		\\ esc for \ etc

	If no paths given on command line
	then will grep from stdin.")
(("-e" "--exp")
	,(lambda (args arg) (setq pattern (first args)) (rest args)))
(("-f" "--file")
	,(lambda (args arg) (setq file_flag :t) args))
(("-w" "--words")
	,(lambda (args arg) (setq words_flag :t) args))
(("-r" "--regexp")
	,(lambda (args arg) (setq regexp_flag :t) args))
(("-c" "--coded")
	,(lambda (args arg) (setq coded_flag :t) args))
(("-m" "--md")
	,(lambda (args arg) (setq md_flag :t) args))
))

;grep a stream to stdout
(defun grep-stream (stream)
	(when stream
		(defq state :nil)
		(each-line (# (task-slice)
			(if md_flag
				(if state
					(if (starts-with "```" %0)
						(setq state :nil))
					(if (starts-with "```" %0)
						(setq state :t)
						(if (. search :match? %0 pattern meta) (print %0))))
				(if (. search :match? %0 pattern meta) (print %0))))
			stream)))

;grep a file to stdout
(defun grep-file (file)
	(when (defq state :nil result :nil stream (file-stream file))
		(while (and (not result) (defq line (read-line stream)))
			(task-slice)
			(if md_flag
				(if state
					(if (starts-with "```" line)
						(setq state :nil))
					(if (starts-with "```" line)
						(setq state :t)
						(if (setq result (. search :match? line pattern meta))
							(print file))))
				(if (setq result (. search :match? line pattern meta))
					(print file))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq pattern "" file_flag :nil words_flag :nil
				regexp_flag :nil coded_flag :nil md_flag :nil
				args (options stdio usage)))
		(when (and (eql pattern "") (> (length args) 1))
			(defq pattern (second args) args (erase args 1 2)))
		(if coded_flag (setq pattern (id-decode pattern)))
		(when (bind '(search pattern meta) (query pattern words_flag regexp_flag))
			(cond
				(file_flag
					;from args ?
					(if (empty? (defq jobs (rest args)))
						;no, so from stdin
						(each-line (# (push jobs %0)) (io-stream 'stdin)))
					(if (<= (length jobs) 1)
						;have do do the work when just 1 file !
						(grep-file (pop jobs))
						;do them all out there, by calling myself !
						(each (lambda ((job result)) (prin result))
							(pipe-farm (map (# (cat (first args)
								" -c -f -e " (id-encode pattern)
								(if words_flag " -w" "") (if regexp_flag " -r" "")
								(if md_flag " -m" "") " " %0)) jobs)))))
				(:t (if (empty? (defq jobs (rest args)))
						;grep stream from stdin
						(grep-stream (io-stream 'stdin))
						;grep stream from args
						(each (# (grep-stream (file-stream %0))) jobs)))))))
