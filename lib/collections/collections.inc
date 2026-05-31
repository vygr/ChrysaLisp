;;;;;;;;;;;;;
; collections
;;;;;;;;;;;;;

(import "./emap.inc")
(import "./fmap.inc")
(import "./fset.inc")
(import "./lset.inc")
(import "./xmap.inc")
(import "./xset.inc")
(import "./lmap.inc")
(import "./tree.inc")

(defun gather (m &rest _)
	; (gather map|set [key] ...) -> (val|key|:nil ...)
	; gather a list of [key|val|:nil]
	(if (list? (first _)) (setq _ (first _)))
	(map (# (. m :find %0)) _))

(defun scatter (m &rest _)
	; (scatter map|set [key]|[key val] ...) -> map|set
	; scatter a list of [key]|[key val]
	(if (list? (first _)) (setq _ (first _)))
	(if (rfind :Set (. m :type_of))
		(reduce (lambda (m k) (. m :insert k)) _ m)
		(reduce (lambda (m (k v)) (. m :insert k v)) (partition _ 2) m)))

(defun tsort (roots dep_fnc)
	; (tsort roots dep_fnc) -> order
	;iterative topological sort using a heap-allocated DFS stack
	(defq visited (Fset 101) visiting (Fset 101)
		order (list) stack (list) e (penv))
	;initialize the stack with the top-level roots
	(each (# (push stack %0 :enter)) roots)
	(while (defq state (pop stack) node (pop stack))
		(task-slice)
		(cond
			((eql state :enter)
				(unless (or (. visited :find node) (. visiting :find node))
					(. visiting :insert node)
					;push the exit state first so it is evaluated after its dependencies
					(push stack node :exit)
					;push all unvisited dependencies onto the stack
					(each (# (unless (or (. visited :find %0) (. visiting :find %0))
							(push stack %0 :enter)))
						(callback dep_fnc e node))))
			((eql state :exit)
				(. visiting :erase node)
				(. visited :insert node)
				(push order node))))
	order)

;;;;;;;;;;;;;;;;;;;;;;;
; Fmap/Lmap memoization
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro memoize (k f &optional num_buckets)
	; (memoize key form [num_buckets]) -> (eval form)
	(if num_buckets
		(static-qq (. ,(Fmap num_buckets) :memoize ,k (# ,f)))
		(static-qq (. ,(Lmap) :memoize ,k (# ,f)))))
