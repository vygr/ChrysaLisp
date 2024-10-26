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
		;scan root for lists and envs
		(defq node_set (Fset 101) len_map (Fmap 3) stack (list *root_env*))
		(while (defq node (pop stack))
			(if (. node_set :inserted node)
				(each (# (if (or (list? %0) (env? %0)) (push stack %0)))
					(if (env? node) (map (const second) (tolist node)) node))))
		;gather length stats
		(. node_set :each (# (when (list? %0)
			(. len_map :update (length %0) (# (if %0 (inc %0) 0))))))
		;display results
		(. len_map :each (# (push stack (list %0 %1))))
		(print "Root environment lists")
		(each (# (print "len: " (pad (first %0) 3) " cnt: " (pad (second %0) 4)))
			(sort stack (# (- (first %0) (first %1)))))
		))
