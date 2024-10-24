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
		(defq node_set (Fset 101) len_map (Fmap 101) res (list))
		;scan root for lists and envs
		(defq stack (list *root_env*))
		(while (defq node (pop stack))
			(unless (. node_set :find node)
				(. node_set :insert node)
				(each (# (if (or (list? %0) (env? %0)) (push stack %0)))
					(if (env? node) (map (const second) (tolist node)) node))))
		;gather length stats
		(. node_set :each (# (when (list? %0)
			(unless (defq cnt (. len_map :find (length %0))) (setq cnt 0))
			(. len_map :insert (length %0) (inc cnt)))))
		;display results
		(. len_map :each (# (push res (list %0 %1))))
		(print "Root environment lists")
		(each (# (print "len: " (pad (first %0) 3) " cnt: " (pad (second %0) 4)))
			(sort res (# (- (first %0) (first %1)))))
		))
