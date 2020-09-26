;imports
(import 'class/lisp.inc)
(import 'sys/lisp.inc)
(import 'lib/options/options.inc)

(defun-bind make-tree (dir ext)
	(defq dirs (list) files (list))
	(each! 0 -1
		(# (unless (starts-with "." %0)
			(cond
				((eql "4" %1) (push dirs (cat dir "/" %0)))
				((ends-with ext %0) (push files (cat dir "/" %0))))))
		(unzip (split (pii-dirlist dir) ",") (list (list) (list))))
	(each (# (setq files (cat files (make-tree %0 ext)))) dirs)
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
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq postfix (if (< (length args) 2) "." (elem 1 args))
			prefix (if (< (length args) 3) "" (elem 2 args)))
		(if (ends-with "/" postfix) (setq postfix (slice 0 -2 postfix)))
		(each print (make-tree postfix prefix))))
