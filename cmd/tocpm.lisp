;imports
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'cmd/options.inc)

(defun conv-file (in_file)
	(defq out_file (cat (slice 0 (find "." in_file) in_file) ".cpm")
		canvas (canvas-load in_file load_flag_noswap))
	(canvas-save canvas out_file format)
	(print in_file " -> " out_file)
	(stream-flush (file-stream 'stdout)))

(defq usage `(
(("-h" "--help")
"Usage: tocmp [options] [path] ...
	options:
		-h --help: this help info.
		+f | ++format 1|16|24|32: default 32.
	Load the images and save as .cpm images.
	If no paths given on command line
	then paths are read from stdin.")
(("+f" "++format")
	,(bind-fun (lambda (o f) (setq format (to-num f)))))
))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq format 32 args (options slave usage)))
	(if (<= (length args) 1)
		;convert from stdin
		(while (defq l (read-line (file-stream 'stdin)))
			(unless (eql l "") (conv-file l)))
		;convert from args
		(each conv-file (slice 1 -1 args))))
