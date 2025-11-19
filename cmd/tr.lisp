(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: tr [options] set1 [set2]

	options:
		-h --help: this help info.
		-d --delete: delete characters in set1, don't translate.
		-s --squeeze: replace repeated characters in set1 with single occurrence.

	Translate or delete characters from stdin to stdout.
	SET1 and SET2 are strings of characters.

	Without -d, translates characters in SET1 to corresponding characters in SET2.")
(("-d" "--delete") ,(opt-flag 'opt_delete))
(("-s" "--squeeze") ,(opt-flag 'opt_squeeze))
))

(defun expand-set (s)
	;simple set expansion - could be enhanced to support ranges like a-z
	s)

(defun translate-char (c from_set to_set)
	(if (/= :nil (defq pos (find c from_set)))
		(if (< pos (length to_set))
			(elem pos to_set)
			(elem -2 to_set))
		c))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_delete :nil opt_squeeze :nil args (options stdio usage)))
		(cond
			;need at least one set
			((<= (length args) 1)
				(print "tr: missing operand"))
			;delete mode
			(opt_delete
				(defq delete_set (expand-set (second args)))
				(defq last_c "")
				(while (defq c (read-char (io-stream 'stdin)))
					(when (eql :nil (find c delete_set))
						(cond
							;squeeze repeated chars
							(opt_squeeze
								(unless (eql c last_c)
									(write-char (io-stream 'stdout) c)
									(setq last_c c)))
							;normal output
							:t
								(write-char (io-stream 'stdout) c)))))
			;translate mode
			((<= (length args) 2)
				(print "tr: two sets required for translation"))
			:t
				(defq from_set (expand-set (second args)))
				(defq to_set (expand-set (elem 2 args)))
				(defq last_c "")
				(while (defq c (read-char (io-stream 'stdin)))
					(defq new_c (translate-char c from_set to_set))
					(cond
						;squeeze repeated chars
						(opt_squeeze
							(unless (eql new_c last_c)
								(write-char (io-stream 'stdout) new_c)
								(setq last_c new_c)))
						;normal output
						:t
							(write-char (io-stream 'stdout) new_c))))
		(stream-flush (io-stream 'stdout))))
