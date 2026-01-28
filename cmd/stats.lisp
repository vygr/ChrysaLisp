(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: stats [options]

	options:
		-h --help: this help info.

	Some simple object statistics.")
))

(defun count-up (%0)
	(ifn %0
		(nums 1 (getf obj +obj_count 0))
		(nums (inc (first %0)) (+ (second %0) (getf obj +obj_count 0)))))

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
		(each (lambda (obj) (cond
				((list? obj) (. list_map :update (length obj) (const count-up)))
				((str? obj) (. str_map :update (length obj) (const count-up)))
				((num? obj) (. num_map :update obj (const count-up)))))
			node_ref)
		(setq node_ref :nil)
		;display :list results
		(. list_map :each (# (push stack (list %0 (first %1) (second %1)))))
		(print "Root environment :list stats")
		(print)
		(each (lambda ((%0 %1 %2)) (print
				"len: " (pad %0 4)
				" cnt: " (pad %1 5)
				" ref: " (pad %2 5)))
			(sort stack (# (- (first %0) (first %1)))))
		(print)
		;display :str results
		(clear stack)
		(. str_map :each (# (push stack (list %0 (first %1) (second %1)))))
		(print "Root environment :str stats")
		(print)
		(each (lambda ((%0 %1 %2)) (print
				"len: " (pad %0 4)
				" cnt: " (pad %1 5)
				" ref: " (pad %2 5)))
			(sort stack (# (- (first %0) (first %1)))))
		(print)
		;display :num results
		(clear stack)
		(. num_map :each (# (push stack (list %0 (first %1) (second %1)))))
		(print "Root environment :num stats")
		(print)
		(each (lambda ((%0 %1 %2)) (print
				"val: " (pad %0 20)
				" cnt: " (pad %1 5)
				" ref: " (pad %2 5)))
			(sort stack (# (- (first %0) (first %1)))))
		))
