(report-header "Sequence Edges: empty, ends, negative and reversed indices")

; --- slice, -1 is the end, a start after the end gives the reverse ---
(test-cases
	(slice "hello" 0 0) ""
	(slice "hello" 0 -1) "hello"
	(slice "hello" 0 5) "hello"
	(slice "hello" 5 5) ""
	(slice "hello" -1 -1) ""
	(slice "hello" -2 -1) "o"
	(slice "hello" 1 -2) "ell"
	(slice "hello" 0 -6) ""
	(slice "hello" 3 1) "le"
	(slice "hello" -1 0) "olleh"
	(slice "" 0 0) ""
	(slice "" 0 -1) ""
	(slice (list 1 2 3) 0 -1) '(1 2 3)
	(slice (list 1 2 3) -1 0) '(3 2 1)
	(slice (list 1 2 3) 2 0) '(2 1)
	(slice (list 1 2 3) 3 3) '()
	(slice (list) 0 -1) '()
	(slice (nums 1 2 3) 1 -1) (nums 2 3)
	(slice (nums 1 2 3) -1 0) (nums 3 2 1))

; --- elem-get with negative indices, -2 is the last element ---
(test-cases
	(elem-get "abc" 0) "a"
	(elem-get "abc" -2) "c"
	(elem-get "abc" -4) "a"
	(elem-get (list 1 2 3) -2) 3
	(elem-get (list 1 2 3) -4) 1
	(elem-set (list 1 2 3) -2 9) '(1 2 9)
	(elem-set (list 1 2 3) 0 9) '(9 2 3))

; --- the ends of an empty or one element sequence ---
(test-cases
	(first (list)) :nil		(first "") :nil
	(last (list)) :nil		(last "") :nil
	(first "a") "a"			(last "a") "a"
	(second (list 1)) :nil	(third (list 1 2)) :nil
	(rest (list)) '()		(rest (list 1)) '()
	(rest "a") ""			(rest "") ""
	(most (list 1)) '()		(most (list)) '()
	(most "abc") "ab"
	(pop (list)) :nil		(pop (list 1)) 1
	(length "") 0			(length (list)) 0		(length (nums)) 0)

; --- cat with empty parts ---
(test-cases
	(cat "") ""
	(cat "" "") ""
	(cat "a" "" "b") "ab"
	(cat (list) (list)) '()
	(cat (list 1) (list) (list 2)) '(1 2)
	(cat (nums 1) (nums 2)) (nums 1 2))

; --- find and rfind, rfind gives the index after the match ---
(test-cases
	(find 2 (list 1 2 3 2)) 1
	(find 9 (list 1 2 3)) :nil
	(find 1 (list)) :nil
	(find "b" "abcb") 1
	(find "z" "abc") :nil
	(find 2 (nums 1 2 3)) 1
	(rfind 2 (list 1 2 3 2)) 4
	(rfind "b" "abcb") 4
	(rfind "z" "abc") :nil)

; --- reverse, partition, range ---
(test-cases
	(reverse (list)) '()
	(reverse (list 1)) '(1)
	(reverse "") ""
	(reverse "abc") "cba"
	(partition (list 1 2 3 4 5) 2) '((1 2) (3 4) (5))
	(partition (list) 2) '()
	(partition (list 1 2 3) 5) '((1 2 3))
	(partition (list 1 2 3 4) 1) '((1) (2) (3) (4))
	(partition "abcdef" 4) '("abcd" "ef")
	(range 0 0) '()
	(range 0 1) '(0)
	(range 5 0) '(5 4 3 2 1)
	(range 0 10 3) '(0 3 6 9)
	(range 10 0 3) '(10 7 4 1)
	;the sign of the step is ignored
	(range 0 5 -1) '(0 1 2 3 4)
	(range -3 3) '(-3 -2 -1 0 1 2))

; --- zip, unzip, flatten, unique, join ---
(test-cases
	(unzip (list 1 2 3 4 5) 2) '((1 3 5) (2 4))
	(unzip (list) 2) '(() ())
	;zip stops at the shortest
	(zip (list 1 2 3) (list 4 5)) '(1 4 2 5)
	(zip (list) (list)) '()
	(flatten (list 1 (list 2 (list 3 (list))) 4)) '(1 2 3 4)
	(flatten (list)) '()
	(flatten (list (list) (list (list)))) '()
	;unique only removes adjacent repeats
	(unique (list 1 1 2 2 2 3 1)) '(1 2 3 1)
	(unique (list)) '()
	(unique "aabbbc") "abc"
	(join (list "a" "b" "c") ",") "a,b,c"
	(join (list "a") ",") "a"
	(join (list "a" "b") "") "ab"
	(join (list) ",") ""
	(join (list) (list 0)) '()
	(join (list (list 1) (list 2)) (list 0)) '(1 0 2))

; --- swap, lists, push, pop, clear ---
(test-cases
	(swap (list 1 2 3) 0 2) '(3 2 1)
	(swap (list 1 2 3) 0 -2) '(3 2 1)
	(swap (list 1 2 3) 1 1) '(1 2 3)
	(lists 3) '(() () ())
	(lists 0) '()
	(push (list) 1 2 3) '(1 2 3)
	(clear (list 1 2) (list 3)) '())

(defq edge_list (list 1 2 3))
(pop edge_list) (pop edge_list) (pop edge_list)
(assert-eq "pop past empty" :nil (pop edge_list))
(assert-eq "pop past empty length" 0 (length edge_list))

; --- insert, erase, replace, rotate at the ends ---
(test-cases
	(insert (list 1 2 3) 0 (list 9)) '(9 1 2 3)
	(insert (list 1 2 3) 3 (list 9)) '(1 2 3 9)
	(insert (list 1 2 3) -1 (list 9)) '(1 2 3 9)
	(insert (list 1 2 3) 1 (list)) '(1 2 3)
	(insert "abc" 1 "XY") "aXYbc"
	(insert "abc" -1 "Z") "abcZ"
	(insert "" 0 "Z") "Z"
	(erase (list 1 2 3 4) 1 3) '(1 4)
	(erase (list 1 2 3 4) 0 -1) '()
	(erase (list 1 2 3 4) 2 2) '(1 2 3 4)
	(erase "abcdef" 0 2) "cdef"
	(erase "abcdef" 4 -1) "abcd"
	(replace (list 1 2 3 4) 1 3 (list 9 9 9)) '(1 9 9 9 4)
	(replace "abcdef" 1 3 "") "adef"
	(replace "abcdef" 0 -1 "x") "x"
	(rotate (list 1 2 3 4 5) 0 2 5) '(3 4 5 1 2)
	(rotate (list 1 2 3 4 5) 0 0 5) '(1 2 3 4 5)
	(rotate (list 1 2 3 4 5) 0 5 5) '(1 2 3 4 5)
	(rotate "abcde" 1 2 4) "acdbe")

; --- emptiness and type predicates ---
(test-cases
	(empty? "") :t			(empty? (list)) :t
	(nempty? "") :nil		(nempty? " ") :t
	(nil? :nil) :t			(nil? (list)) :nil		(nil? 0) :nil
	(atom? 1) :t			(atom? "a") :t			(atom? (list)) :nil
	(type-of (list)) '(:seq :array :list)
	(type-of "a") '(:seq :str)
	(type-of 'a) '(:seq :str :sym)
	(type-of 1) '(:num)
	(type-of 1.0) '(:num :fixed)
	(type-of (nums)) '(:seq :array :nums))

;predicates give :nil or some non :nil value, not always :t
(assert-true "num? num" (num? 5))
(assert-true "num? fixed" (num? 5.0))
(assert-true "num? str" (not (num? "5")))
(assert-true "array? list" (array? (list)))
(assert-true "array? nums" (array? (nums)))
(assert-true "array? str" (not (array? "a")))
(assert-true "seq? str" (seq? "a"))
(assert-true "seq? num" (not (seq? 5)))
(assert-true "list? list" (list? (list)))
(assert-true "list? str" (not (list? "a")))
(assert-true "str? empty" (str? ""))
(assert-true "str? sym" (str? 'a))
(assert-true "sym? sym" (sym? 'a))
(assert-true "sym? keyword" (sym? :a))
(assert-true "sym? str" (not (sym? "a")))

; --- eql, content for values and vectors, identity for lists ---
(test-cases
	(eql "a" "a") :t			(eql "" "") :t
	(eql "a" 'a) :nil			(eql 'a 'a) :t
	(eql 1 1) :t				(eql 1 1.0) :nil
	(eql 1.0 1.0) :t
	(eql (nums 1 2) (nums 1 2)) :t
	(eql (nums 1 2) (nums 1 3)) :nil
	(eql (nums 1 2) (list 1 2)) :nil
	(eql :nil (list)) :nil		(eql :nil :nil) :t
	(nql 1 2) :t				(nql 1 1) :nil)
