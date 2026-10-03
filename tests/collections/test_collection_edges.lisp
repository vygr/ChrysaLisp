(report-header "Collection Edges: empty maps and sets, repeated keys, set algebra")

; --- an empty map ---
(defq ce_m (Fmap))
(assert-eq "empty map find" :nil (. ce_m :find 'a))
(assert-eq "empty map size" 0 (. ce_m :size))
(assert-true "empty map empty?" (. ce_m :empty?))
(assert-list-eq "empty map tolist" '() (. ce_m :tolist))
(. ce_m :erase 'a)
(assert-eq "erase a missing key" 0 (. ce_m :size))
(defq ce_count 0)
(. ce_m :each (lambda (k v) (++ ce_count)))
(assert-eq "empty map each" 0 ce_count)

; --- the same key again replaces, it does not add ---
(. ce_m :insert 'a 1)
(. ce_m :insert 'a 2)
(assert-eq "insert again value" 2 (. ce_m :find 'a))
(assert-eq "insert again size" 1 (. ce_m :size))
(assert-list-eq "one entry tolist" '((a 2)) (. ce_m :tolist))
(. ce_m :erase 'a)
(assert-eq "erase then find" :nil (. ce_m :find 'a))
(assert-true "erase then empty?" (. ce_m :empty?))

;a :nil value is stored, but can not be told from a missing key by :find
(. ce_m :insert 'n :nil)
(assert-eq "nil value find" :nil (. ce_m :find 'n))
(assert-eq "nil value size" 1 (. ce_m :size))
(. ce_m :empty)

; --- keys of other types, a number and a string of it are different keys ---
(. ce_m :insert 1 :num)
(. ce_m :insert "1" :str)
(. ce_m :insert "" :empty)
(. ce_m :insert (list 1 2) :list)
(assert-eq "num key" :num (. ce_m :find 1))
(assert-eq "str key" :str (. ce_m :find "1"))
(assert-eq "empty str key" :empty (. ce_m :find ""))
(assert-eq "list key by content" :list (. ce_m :find (list 1 2)))
(assert-eq "mixed keys size" 4 (. ce_m :size))

; --- one bucket still works, and so does a resize down to one ---
(defq ce_m (Fmap 1))
(each (# (. ce_m :insert %0 (* %0 %0))) (range 0 50))
(assert-eq "one bucket size" 50 (. ce_m :size))
(assert-eq "one bucket last" 2401 (. ce_m :find 49))
(assert-eq "one bucket missing" :nil (. ce_m :find 50))
(defq ce_m (Fmap 3))
(each (# (. ce_m :insert %0 %0)) (range 0 10))
(. ce_m :resize 1)
(assert-eq "resize keeps size" 10 (. ce_m :size))
(assert-eq "resize keeps entries" 9 (. ce_m :find 9))

; --- copy is independent of the original ---
(defq ce_m (Fmap))
(. ce_m :insert 'a 1)
(defq ce_m2 (. ce_m :copy))
(. ce_m :empty)
(assert-eq "copy original emptied" 0 (. ce_m :size))
(assert-eq "copy kept" 1 (. ce_m2 :size))

; --- update and memoize on a missing key ---
(defq ce_m (Fmap))
(. ce_m :update 'a (lambda (v) (ifn v 1 (inc v))))
(. ce_m :update 'a (lambda (v) (ifn v 1 (inc v))))
(assert-eq "update from missing" 2 (. ce_m :find 'a))
(. ce_m :memoize 'b (lambda () :nil))
(assert-eq "memoize of :nil is stored" 2 (. ce_m :size))

; --- Emap and Lmap ---
(defq ce_e (Emap))
(. ce_e :insert 'a 1)
(. ce_e :insert :b 2)
(assert-list-eq "Emap finds" '(1 2 :nil) (list (. ce_e :find 'a) (. ce_e :find :b) (. ce_e :find 'c)))
(assert-eq "Emap size" 2 (. ce_e :size))
(defq ce_l (Lmap))
(. ce_l :insert 'a 1)
(. ce_l :insert 'b 2)
(. ce_l :insert 'a 3)
(assert-list-eq "Lmap keeps insert order" '((a 3) (b 2)) (. ce_l :tolist))
(. ce_l :erase 'a)
(assert-list-eq "Lmap erase" '((b 2)) (. ce_l :tolist))

; --- an empty set, and the same key twice ---
(defq ce_s (Fset))
(assert-eq "empty set find" :nil (. ce_s :find 'a))
(assert-eq "empty set size" 0 (. ce_s :size))
(assert-true "empty set empty?" (. ce_s :empty?))
(. ce_s :erase 'a)
(assert-eq "set erase a missing key" 0 (. ce_s :size))
(assert-true "inserted first time" (. ce_s :inserted 'a))
(assert-true "inserted second time" (not (. ce_s :inserted 'a)))
(. ce_s :insert 'a)
(assert-eq "set insert again size" 1 (. ce_s :size))
(. ce_s :insert "k")
(assert-true "intern gives the one object" (eql (. ce_s :intern "k") (. ce_s :intern "k")))

; --- set algebra, with overlapping, empty, and the same set ---
(defun ce-set (&rest keys)
	(defq s (Fset))
	(each (# (. s :insert %0)) keys)
	s)

(defun ce-keys (s)
	(sort (. s :tolist) (const -)))

(test-cases
	(ce-keys (. (ce-set 1 2) :union (ce-set 2 3))) '(1 2 3)
	(ce-keys (. (ce-set 1 2) :difference (ce-set 2 3))) '(1)
	(ce-keys (. (ce-set 1 2) :intersect (ce-set 2 3))) '(2)
	(ce-keys (. (ce-set 1 2) :not_intersect (ce-set 2 3))) '(1 3)
	(ce-keys (. (ce-set 1) :union (ce-set))) '(1)
	(ce-keys (. (ce-set) :union (ce-set 1))) '(1)
	(ce-keys (. (ce-set 1) :intersect (ce-set))) '()
	(ce-keys (. (ce-set 1) :difference (ce-set))) '(1)
	(ce-keys (. (ce-set) :difference (ce-set 1))) '())

(defq ce_self (ce-set 1))
(assert-list-eq "union with itself" '(1) (ce-keys (. ce_self :union ce_self)))
(assert-list-eq "intersect with itself" '(1) (ce-keys (. ce_self :intersect ce_self)))
(assert-list-eq "difference with itself" '() (ce-keys (. ce_self :difference ce_self)))

; --- pmap and pset ---
(test-cases
	(pfind (pmap) 'a) :nil
	(pfind (pinsert (pmap) 'a 1) 'a) 1
	(pfind (pinsert (pmap) 'a 1) 'b) :nil
	(pfind (pinsert (pinsert (pmap) 'a 1) 'a 2) 'a) 2
	;a pmap holds key and value, so one entry has length 2
	(length (pinsert (pinsert (pmap) 'a 1) 'a 2)) 2
	(length (perase (pinsert (pmap) 'a 1) 'a)) 0
	(length (perase (pmap) 'a)) 0
	(length (pinsert (pinsert (pset) 'a) 'a)) 1
	(pfind (pinsert (pset) 'a) 'a) 'a
	(pfind (pset) 'a) :nil)

(assert-true "pmap? pmap" (pmap? (pmap)))
(assert-true "pmap? list" (not (pmap? (list))))
(assert-true "pset? pset" (pset? (pset)))

; --- scatter and gather ---
(test-cases
	(gather (scatter (Fmap) 'a 1 'b 2) 'a 'b 'c) '(1 2 :nil)
	(gather (Fmap) 'a) '(:nil)
	(gather (scatter (Fmap) 'a 1)) '())
