(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: cp [options] path1 path2

	options:
		-h --help: this help info.

	Copy file path1 to path2.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(= (length args) 3))
		(bind '(src dst) (rest args))
		(setq src (file-stream src) dst (file-stream dst +file_open_write))
		(while (defq c (read-char src)) (write-char dst c))))
