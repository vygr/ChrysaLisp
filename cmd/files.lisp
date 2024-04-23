(import "lib/options/options.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
{Usage: files [options] [prefix] [postfix]

	options:
		-h --help: this help info.
		-i --imm: immediate dependencies.
		-a --all: all dependencies.
		-d --dirs: directories.

	Find all paths that match the prefix and postfix.

		prefix default "."
		postfix default ""

	eg.
	files -a apps/ .lisp})
(("-d" "--dirs")
	,(lambda (args arg) (setq opt_d :t) args))
(("-i" "--imm:")
	,(lambda (args arg) (setq opt_i :t) args))
(("-a" "--all")
	,(lambda (args arg) (setq opt_a :t) args))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_d :nil opt_i :nil opt_a :nil
				args (options stdio usage)))
		(defq postfix (if (< (length args) 2) "." (second args))
			prefix (if (< (length args) 3) "" (third args)))
		(if (ends-with "/" postfix) (setq postfix (most postfix)))
		(each (const print) (usort
			(reduce (lambda (files (option func)) (if option (func files) files))
				(list
					(list opt_i (# (usort (flatten (map (const files-depends) %0)))))
					(list opt_a (const files-all-depends))
					(list opt_d (const files-dirs)))
				(map (# (if (starts-with "./" %0) (slice %0 2 -1) %0))
					(files-all postfix `(,prefix))))))))
