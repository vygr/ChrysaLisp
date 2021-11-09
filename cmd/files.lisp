(import "class/lisp.inc")
(import "sys/lisp.inc")
(import "lib/options/options.inc")

(defun all-files (root ext)
	;all files from root downwards with extention, none recursive
	;don't include "." folders
	(defq stack (list root) files (list))
	(while (setq root (pop stack))
		(each! 0 -1 (lambda (file type)
			(unless (starts-with "." file)
				(if (eql type "4")
					(push stack (cat root "/" file))
					(if (ends-with ext file)
						(push files (cat root "/" file))))))
			(unzip (split (pii-dirlist root) ",") (list (list) (list)))))
	files)

(defq usage `(
(("-h" "--help")
{Usage: file [options] [prefix] [postfix]
	options:
		-h --help: this help info.
	Find all files that match the prefix and postfix.
		prefix default ".".
		postfix default "".
	eg.
	files ./apps/wallpaper/ .tga})
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq postfix (if (< (length args) 2) "." (elem-get 1 args))
			prefix (if (< (length args) 3) "" (elem-get 2 args)))
		(if (ends-with "/" postfix) (setq postfix (slice 0 -2 postfix)))
		(each print (all-files postfix prefix))))
