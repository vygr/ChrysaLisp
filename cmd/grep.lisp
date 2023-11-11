(import "lib/options/options.inc")
(import "lib/text/regexp.inc")

;grep a stream to stdout
(defun grep-file (stream)
	(each-line (# (if (. regexp :match? %0 pattern cexp) (print %0)))
		stream))

(defq usage `(
(("-h" "--help")
"Usage: grep [options] [pattern] [path] ...

	options:
		-h --help: this help info.
		-e --exp pattern: regular expression.

	pattern:
		^  start of line
		$  end of line
		{  start of word
		}  end of word
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
	then will grep stdin.")
(("-e" "--exp")
	,(lambda (args arg)
		(setq pattern (first args))
		(rest args)))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq pattern "" args (options stdio usage)))
		(when (and (eql pattern "") (> (length args) 1))
			(defq pattern (second args) args (erase args 1 2)))
		(when (defq regexp (Regexp) cexp (. regexp :compile pattern)))
			(if (<= (length args) 1)
				;grep from stdin
				(grep-file (io-stream 'stdin))
				;grep from args as files
				(each (# (grep-file (file-stream %0))) (rest args)))))
