(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: stats [options]

	options:
		-h --help: this help info.

	Some simple object statistics.")
))

(defun count-up (%0) (if %0 (inc %0) 1))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;scan root for all objects, with cycle protection
		(defq node_set (Fset 501) node_ref (list) stack (list *root_env*)
			list_map (Fmap 31) num_map (Fmap 31) str_map (Fmap 31))
		(while (defq node (pop stack))
			(when (. node_set :inserted (weak-ref node))
				(push node_ref node)
				(if (env? node)
					(setq node (map (const second) (tolist node))))
				(if (list? node)
					(each (# (if %0 (push stack %0))) node))))
		(setq node_set :nil)
		;gather stats
		(each (# (cond
				((list? %0) (. list_map :update (length %0) (const count-up)))
				((str? %0) (. str_map :update (length %0) (const count-up)))
				((num? %0) (. num_map :update %0 (const count-up)))))
			node_ref)
		(setq node_ref :nil)
		;display :list results
		(. list_map :each (# (push stack (list %0 %1))))
		(print "Root environment :list stats")
		(print)
		(each (# (print
				"len: " (pad (first %0) 4)
				" cnt: " (pad (second %0) 5)))
			(sort stack (# (- (first %0) (first %1)))))
		(print)
		;display :str results
		(clear stack)
		(. str_map :each (# (push stack (list %0 %1))))
		(print "Root environment :str stats")
		(print)
		(each (# (print
				"len: " (pad (first %0) 4)
				" cnt: " (pad (second %0) 5)))
			(sort stack (# (- (first %0) (first %1)))))
		(print)
		;display :num results
		(clear stack)
		(. num_map :each (# (push stack (list %0 %1))))
		(print "Root environment :num stats")
		(print)
		(each (# (print
				"val: " (pad (first %0) 20)
				" cnt: " (pad (second %0) 5)
				" ref: " (pad (getf (first %0) +obj_count 0) 5)))
			(sort stack (# (- (first %0) (first %1)))))
		))
