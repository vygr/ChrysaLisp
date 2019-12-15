;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: echo [options] arg ...
	options:
		-h --help: this help info.")
))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq args (options slave usage)))
	(print (apply cat (map (lambda (_) (cat _ " ")) (slice 1 -1 args)))))
