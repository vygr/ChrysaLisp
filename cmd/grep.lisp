(import "class/lisp.inc")
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
		\s  space
		\q  double quote
		\t  tab
		\r  return
		\f  form feed
		\n  line feed
		\w  white space
		\b  black space
		\\  esc for \ etc
		.   any char
		+   one or more
		*   zero or more
		?   zero or one
		|   or
		^   start of line
		$   end of line
		{   start of word
		}   end of word
		[]  class, [0-9], [abc123]
		()  group

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
