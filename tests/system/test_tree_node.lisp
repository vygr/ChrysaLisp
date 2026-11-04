(report-header "System: Tree-node, a tree of objects")

(import "lib/collections/tree_node.inc")

(defq tn_top (Tree-node "top") tn_a (Tree-node "a") tn_b (Tree-node "b") tn_c (Tree-node "a2"))
(.-> tn_top (:add_node tn_a) (:add_node tn_b))
(. tn_a :add_node tn_c)
(assert-list-eq "children, in the order added" '("a" "b") (map (# (. %0 :get_name)) (. tn_top :children)))
(assert-eq "a child's parent" tn_top (. tn_a :get_parent))
(assert-eq "the root of a grandchild" tn_top (. tn_c :root))
(assert-eq "the root of the root" tn_top (. tn_top :root))
(assert-list-eq "found by the start of a name, in walk order" '("a" "a2")
	(map (# (. %0 :get_name)) (. tn_top :find_nodes "a")))

;the walk goes in to each node and out of it, in and out balanced
(defq tn_said (list))
(. tn_top :walk_nodes
	(lambda (node children) (push tn_said (cat "+" (. node :get_name))) :nil)
	(lambda (node children) (push tn_said (cat "-" (. node :get_name)))))
(assert-list-eq "a walk in and out" '("+top" "+a" "+a2" "-a2" "-a" "+b" "-b" "-top") tn_said)
;one that says :t is not stepped into
(setq tn_said (list))
(. tn_top :walk_nodes
	(lambda (node children) (push tn_said (. node :get_name)) (eql (. node :get_name) "a"))
	(lambda (node children)))
(assert-list-eq "not stepped into" '("top" "a" "b") tn_said)
;each node, a parent before its children, they in order
(setq tn_said (list))
(. tn_top :each_node (lambda (node) (push tn_said (. node :get_name))))
(assert-list-eq "each node, in walk order" '("top" "a" "a2" "b") tn_said)

;added to another, it leaves the one it was in
(. tn_b :add_node tn_c)
(assert-eq "moved, out of the old parent" 0 (length (. tn_a :children)))
(assert-eq "and in the new" tn_b (. tn_c :get_parent))
(. tn_c :sub_node)
(assert-eq "taken out, no parent" :nil (. tn_c :get_parent))
(assert-eq "and not a child" 0 (length (. tn_b :children)))
(assert-eq "taken out again does nothing" tn_c (. tn_c :sub_node))

;its children set whole, each given this as its parent, those it had let go
(. tn_top :set_children (list tn_c))
(assert-list-eq "set, the new" (list tn_c) (. tn_top :children))
(assert-eq "its parent" tn_top (. tn_c :get_parent))
(assert-eq "those it had have none" :nil (. tn_a :get_parent))
