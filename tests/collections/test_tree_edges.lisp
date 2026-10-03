(import "lib/collections/tree.inc")

(report-header "Tree Edges: empty collections and awkward values through save and load")

(defun te-round (tree)
	; save the tree to a stream and load it back
	(defq ms (memory-stream))
	(tree-save ms tree)
	(stream-seek ms 0 0)
	(tree-load ms))

(defun te-value (val)
	; a value, saved and loaded back as an entry in a map
	(defq m (Fmap))
	(. m :insert :k val)
	(. (te-round m) :find :k))

; --- empty collections ---
(test-cases
	(. (te-round (Fmap)) :size) 0
	(. (te-round (Emap)) :empty?) :t
	(. (te-round (Fset)) :size) 0
	(. (te-value (Fmap)) :size) 0
	(te-value (list)) '())

; --- values that need care in the file ---
(test-cases
	(te-value "") ""
	(te-value -5) -5
	(te-value -1.5) -1.5
	(te-value :t) :t
	(te-value 'a_symbol) 'a_symbol
	(te-value "a b\tc") "a b\tc"
	(te-value "line1\nline2") "line1\nline2"
	(te-value "say \qhi\q") "say \qhi\q"
	(te-value "{brace}") "{brace}"
	(te-value "(paren)") "(paren)"
	(te-value "[square]") "[square]"
	(te-value "back\\slash") "back\\slash"
	(te-value "; not a comment") "; not a comment"
	(te-value (list 1 (list 2 (list)) "x" :s)) '(1 (2 ()) "x" :s))

;a string too long for one chunk is split, and joined on load
(defq te_long (apply (const cat) (map (# (str %0 ",")) (range 0 400))))
(assert-eq "long string round trip" te_long (te-value te_long))
(assert-true "long string is long" (> (length te_long) 1000))

; --- keys that are not symbols ---
(defq te_m (Fmap))
(. te_m :insert "str key" 1)
(. te_m :insert 7 2)
(defq te_l (te-round te_m))
(assert-eq "string key" 1 (. te_l :find "str key"))
(assert-eq "number key" 2 (. te_l :find 7))

; --- an Lmap keeps its order ---
(defq te_m (Lmap))
(. te_m :insert :b 1)
(. te_m :insert :a 2)
(assert-list-eq "Lmap order kept" '((:b 1) (:a 2)) (. (te-round te_m) :tolist))

; --- a set of values ---
(defq te_s (Fset))
(each (# (. te_s :insert %0)) (list 1 "two" :three))
(defq te_l (te-round te_s))
(assert-eq "set size" 3 (. te_l :size))
(assert-true "set str member" (. te_l :find "two"))
(assert-true "set sym member" (. te_l :find :three))

; --- nothing to load gives :nil ---
(test-cases
	(tree-load :nil) :nil
	(tree-load (memory-stream)) :nil
	(tree-load (string-stream "")) :nil
	(tree-load (string-stream "  \n")) :nil
	(tree-load (string-stream "; just a comment")) :nil)

;a nums vector is not a tree value
(assert-error "nums value" (tree-save (memory-stream) (te-value (nums 1 2))))
