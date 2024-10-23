(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: stats [options]

	options:
		-h --help: this help info.

	Some simple object statistics.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq list_set (Fset 101) len_map (Fmap 101) res (list))
		;scan root env for lists
		(each (lambda ((s o))
				(when (list? o)
					(. list_set :insert o)
					(defq stack (list o))
					(while (defq lst (pop stack))
						(each (#
							(when (list? %0)
								(unless (. list_set :find %0)
									(. list_set :insert %0)
									(push stack %0))))
							lst))))
			(tolist *root_env*))
		;gather length stats
		(. list_set :each (#
			(unless (defq cnt (. len_map :find (length %0))) (setq cnt 0))
			(. len_map :insert (length %0) (inc cnt))))
		;display results
		(. len_map :each (# (push res (list %0 %1))))
		(print "Root environment lists")
		(each (# (print "len: " (pad (first %0) 3) " cnt: " (pad (second %0) 4)))
			(sort res (# (- (first %0) (first %1)))))
		))
