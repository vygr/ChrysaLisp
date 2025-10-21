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
		\q double quote
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
(("-e" "--exp") ,(opt-str 'pattern))
(("-f" "--file") ,(opt-flag 'opt_f))
(("-w" "--words") ,(opt-flag 'opt_w))
(("-r" "--regexp") ,(opt-flag 'opt_r))
(("-c" "--coded") ,(opt-flag 'opt_c))
(("-m" "--md") ,(opt-flag 'opt_m))
))

;grep a stream to stdout
(defun grep-stream (stream)
	(when stream
		(defq state :nil)
		(lines! (# (task-slice)
			(if opt_m
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
			(if opt_m
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
			(defq pattern "" opt_f :nil opt_w :nil
				opt_r :nil opt_c :nil opt_m :nil
				args (options stdio usage)))
		(when (and (eql pattern "") (> (length args) 1))
			(defq pattern (second args) args (erase args 1 2)))
		(if opt_c (setq pattern (id-decode pattern)))
		(when (bind '(search pattern meta) (query pattern opt_w opt_r))
			(cond
				(opt_f
					;from args ?
					(if (empty? (defq jobs (rest args)))
						;no, so from stdin
						(lines! (# (push jobs %0)) (io-stream 'stdin)))
					(if (<= (length jobs) 1)
						;have to do the work when just 1 file !
						(grep-file (pop jobs))
						;do them all out there, by calling myself !
						(each (lambda ((job result)) (prin result))
							(pipe-farm (map (# (cat (first args)
								" -c -f -e " (id-encode pattern)
								(if opt_w " -w" "") (if opt_r " -r" "")
								(if opt_m " -m" "") " " %0)) jobs)))))
				(:t (if (empty? (defq jobs (rest args)))
						;grep stream from stdin
						(grep-stream (io-stream 'stdin))
						;grep stream from args
						(each (# (grep-stream (file-stream %0))) jobs)))))))
