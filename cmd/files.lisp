;imports
(import 'class/lisp.inc)
(import 'sys/lisp.inc)
(import 'cmd/options.inc)

(defun-bind make-tree (dir ext)
	(defq dirs (list) files (list))
	(each! 0 -1 (lambda (f d)
		(unless (or (starts-with "." f) (ends-with "." f))
			(cond
				((eql "4" d)
					(push dirs (cat dir "/" f)))
				((ends-with ext f)
					(push files (cat dir "/" f))))))
		(unzip (split (pii-dirlist dir) ",") (list (list) (list))))
	(each (lambda (d)
		(setq files (cat files (make-tree d ext)))) dirs)
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

(defun-bind main ()
	;initialize pipe details and command args, abort on error
	(when (and (defq stdio (create-stdio)) (defq args (options stdio usage)))
		(defq postfix (if (< (length args) 2) "." (elem 1 args))
			prefix (if (< (length args) 3) "" (elem 2 args)))
		(if (ends-with "/" postfix) (setq postfix (slice 0 -2 postfix)))
		(each print (make-tree postfix prefix))))
