;;;;;;;;;;;
; tree node
;;;;;;;;;;;

;A node has its children, in the order they were added, and a weak link to
;its parent: the tree owns down, from the top, and letting go of the top
;lets go of all of it. Classes that are trees of things, a scene of
;objects, a figure of figures, are made of it, and add what they are.
;Its keys begin :node_, as a class made of it keeps more in the same place

;module
(env-push)

(defclass Tree-node (&optional name) :nil
	; (Tree-node [name]) -> tree_node
	(def this :node_children (list) :node_parent :nil :node_name (ifn name ""))

	(defmethod :get_parent ()
		; (. tree_node :get_parent) -> tree_node | :nil
		(obj-ref (get :node_parent this)))

	(defmethod :set_parent (parent)
		; (. tree_node :set_parent parent) -> tree_node
		(def this :node_parent (weak-ref parent))
		this)

	(defmethod :children ()
		; (. tree_node :children) -> children
		(get :node_children this))

	(defmethod :get_name ()
		; (. tree_node :get_name) -> str
		(get :node_name this))

	(defmethod :add_node (child)
		; (. tree_node :add_node child) -> tree_node
		;a child added last, out of the parent it had first
		(if (. child :get_parent) (. child :sub_node))
		(push (get :node_children this) child)
		(. child :set_parent this)
		this)

	(defmethod :set_children (children)
		; (. tree_node :set_children children) -> tree_node
		;these are its children, in place of those it had, and none of
		;another's: each is given this as its parent
		(each (# (. %0 :set_parent :nil)) (get :node_children this))
		(each (# (. %0 :set_parent this)) children)
		(def this :node_children children)
		this)

	(defmethod :sub_node ()
		; (. tree_node :sub_node) -> tree_node
		;this one taken out of its parent
		(when (defq parent (. this :get_parent))
			(. this :set_parent :nil)
			(defq children (get :node_children parent) idx (find this children))
			(if idx (def parent :node_children (erase children idx (inc idx)))))
		this)

	(defmethod :root ()
		; (. tree_node :root) -> tree_node
		;the top of the tree it is in
		(defq node this)
		(while (defq up (. node :get_parent)) (setq node up))
		node)

	(defmethod :walk_nodes (fi fo)
		; (. tree_node :walk_nodes fnc_in fnc_out) -> tree_node
		;if fnc_in returns :nil, it'll step down into that nodes children.
		;fnc_out is always called to balance calls to fnc_in.
		(defq stack (list this (get :node_children this) 0))
		(while (defq idx (pop stack) children (pop stack) this (pop stack))
			(cond
				((and (= idx 0) (callback fi (penv) this children))
					;not stepping in
					(callback fo (penv) this children))
				((< idx (length children))
					;step into child node
					(push stack this children (inc idx)
						(defq this (elem-get children idx)) (get :node_children this) 0))
				(:t ;step out
					(callback fo (penv) this children))))
		this)

	(defmethod :each_node (fnc)
		; (. tree_node :each_node fnc) -> tree_node
		; (fnc node)
		;fnc of this node and of every one under it, a parent before its
		;children and they in order. What is to come is a stack, the depth
		;of the tree costs nothing. fnc is called from inside it, so the
		;names it uses are not these: fnc stack node. The children are pushed
		;last first, so the first comes off first, by (reduce!) of push,
		;which gives back the stack to pop the next from: 10.9 us a walk of
		;111, (reach) and a push 12.5, a loop of (elem-get) 18
		(defq node this stack (list))
		(while node
			(fnc node)
			(setq node (pop (reduce! (const push) (list (get :node_children node)) stack -1 0))))
		this)

	(defmethod :find_nodes (name)
		; (. tree_node :find_nodes name) -> nodes
		;it and those under it whose names begin so, in the order of a walk
		(defq nodes (list))
		(. this :each_node (lambda (n) (if (starts-with name (get :node_name n)) (push nodes n))))
		nodes)
	)

;module
(export-classes '(Tree-node))
(env-pop)
