(report-header "Iteration Edges: empty, ranges, reversed ranges, multiple sequences")

; --- map, filter, reduce over nothing and over strings ---
(test-cases
	(map (# (* %0 2)) (list)) '()
	(map (# (* %0 2)) "") '()
	(map (# %0) "abc") '("a" "b" "c")
	(filter (# (> %0 1)) (list)) '()
	(filter (# (> %0 1)) (list 1 2 3)) '(2 3)
	(filter (lambda (x) :nil) (list 1 2 3)) '()
	(filter (# (eql %0 "a")) "banana") '("a" "a" "a")
	(reduce (# (+ %0 %1)) (list) 0) 0
	(reduce (# (+ %0 %1)) (list 5) 0) 5
	;with no initial value the first element is used
	(reduce (# (+ %0 %1)) (list 1 2 3)) 6
	(reduce (# (cat %0 %1)) "abc" "") "abc"
	(reduce max (list 3 9 2) 0) 9
	(reduce (lambda (acc x) (if (> x acc) x acc)) (nums 3 9 2) 0) 9
	(reduce (# (push %0 %1)) "abc" (list)) '("a" "b" "c")
	(rreduce (# (cat %0 (str %1))) (list 1 2 3) "") "321"
	(rmap (# %0) (list)) '()
	(map (const str) (list 1 :a "b")) '("1" ":a" "b"))

; --- some, every and friends, and what they give on an empty sequence ---
(test-cases
	(some (# (if (> %0 1) %0)) (list 1 2 3)) 2
	(some (# (if (> %0 5) %0)) (list 1 2 3)) :nil
	(some (# %0) (list)) :nil
	(some (# %0) (list :nil :nil)) :nil
	(rsome (# (if (> %0 1) %0)) (list 1 2 3)) 3
	(every (# (> %0 0)) (list)) :t
	(every (# (> %0 0)) (list 1 2 3)) :t
	(every (# (> %0 1)) (list 1 2 3)) :nil
	(notany (# (> %0 5)) (list)) :t
	(notevery (# (> %0 5)) (list)) :nil)

; --- several sequences, stops at the shortest ---
(test-cases
	(map (lambda (a b) (+ a b)) (list 1 2 3) (list 10 20)) '(11 22)
	(map (lambda (a b) (+ a b)) (list) (list 10 20)) '()
	(map list (list 1 2) "ab") '((1 "a") (2 "b"))
	(some (lambda (x y) (if (= x y) x)) (list 1 2 3) (list 3 2 1)) 2
	(every (lambda (x y) (< x y)) (list 1 2) (list 2 3 4)) :t)

; --- the loop index (!), in forward and reverse iteration ---
(test-cases
	(map (lambda (x) (!)) (list 7 8 9)) '(0 1 2)
	(rmap (lambda (x) (!)) (list 7 8 9)) '(2 1 0))

(defq it_out (list))
(each (# (push it_out (!) %0)) "ab")
(assert-list-eq "each index" '(0 "a" 1 "b") it_out)
(clear it_out)
(reach (# (push it_out (!) %0)) "ab")
(assert-list-eq "reach index" '(1 "b" 0 "a") it_out)
(clear it_out)
(each (# (push it_out %0)) (list))
(assert-list-eq "each empty" '() it_out)

; --- each! ranges, an end before the start runs backwards ---
(defun it-range (&rest range)
	(defq out (list))
	(apply each! (cat (list (# (push out %0)) (list (list 1 2 3 4 5))) range))
	out)

(test-cases
	(it-range 1 3) '(2 3)
	(it-range 3 1) '(3 2)
	(it-range 2) '(3 4 5)
	(it-range 0 -2) '(1 2 3 4)
	(it-range -1 0) '(5 4 3 2 1)
	(it-range 2 2) '())

(clear it_out)
(each! (lambda (x) (push it_out (!))) (list (list 1 2 3 4 5)) 3 1)
(assert-list-eq "each! reverse index" '(2 1) it_out)

; --- map!, some!, reduce!, filter! with ranges ---
(test-cases
	(map! (# (* %0 2)) (list (list 1 2 3 4)) 1 3) '(4 6)
	(map! (# (* %0 2)) (list (list 1 2 3 4)) 3 1) '(6 4)
	(map! (# (* %0 2)) (list (list 1 2 3 4)) 2 2) '()
	;the output list can be given, and is appended to
	(map! (# (* %0 2)) (list (list 1 2 3 4)) 0 -1 (list 0)) '(0 2 4 6 8)
	(map! (lambda (a b) (+ a b)) (list (list 1 2 3) (nums 10 20 30)) 1) '(22 33)
	(some! (# (if (> %0 2) %0)) (list (list 1 2 3 4))) 3
	(some! (# (if (> %0 2) %0)) (list (list 1 2 3 4)) :nil 0 2) :nil
	(some! (# (if (> %0 2) %0)) (list (list 1 2 3 4)) :nil 4 0) 4
	(some! (# (if (> %0 9) %0)) (list (list 1 2 3 4))) :nil
	(reduce! (# (+ %0 %1)) (list (list 1 2 3 4)) 0 1 3) 5
	(reduce! (# (+ %0 %1)) (list (list 1 2 3 4)) 0 3 1) 5
	(reduce! (# (+ %0 %1)) (list (list 1 2 3 4)) 0 2 2) 0
	(filter! (# (> %0 2)) (list 1 2 3 4)) '(3 4))

; --- an error thrown inside an iteration must not upset the loop around it ---
;thrown errors are only caught by the iterators on an error checked build
(defun it-nested (thrower)
	; run thrower inside a catch, inside an each, and give the items and indices seen
	(defq out (list))
	(each (lambda (x) (catch (thrower) :t) (push out x (!))) (list 1 2 3))
	out)

(if *test_checked*
	(test-cases
		(it-nested (lambda () (each (lambda (y) (throw "e" 1)) (list 7 8)))) '(1 0 2 1 3 2)
		(it-nested (lambda () (map (lambda (y) (throw "e" 1)) (list 7 8)))) '(1 0 2 1 3 2)
		(it-nested (lambda () (filter (lambda (y) (throw "e" 1)) (list 7 8)))) '(1 0 2 1 3 2)
		(it-nested (lambda () (some (lambda (y) (throw "e" 1)) (list 7 8)))) '(1 0 2 1 3 2)
		(it-nested (lambda () (reduce (lambda (a y) (throw "e" 1)) (list 7 8) 0))) '(1 0 2 1 3 2)
		;a bad argument list to the function, as a macro expansion can give
		(it-nested (lambda () (map (lambda ((a b)) a) (list (list 1))))) '(1 0 2 1 3 2)
		(it-nested (lambda () (sort (list 3 1 2) (lambda (a b) (throw "e" 1))))) '(1 0 2 1 3 2))
	(test-skip "errors inside iteration" "needs an error checked build"))

; --- times, while, until ---
(defq it_count 0)
(times 0 (++ it_count))
(assert-eq "times 0" 0 it_count)
(times 3 (++ it_count))
(assert-eq "times 3" 3 it_count)
(assert-eq "while never" :nil (while (< it_count 3) (++ it_count)))
(setq it_count 0)
(assert-eq "while result" :nil (while (< it_count 3) (++ it_count)))
(assert-eq "while count" 3 it_count)
(setq it_count 0)
(assert-eq "until result" :t (until (>= it_count 3) (++ it_count)))

; --- sort, the default compare is cmp, for strings ---
(test-cases
	(sort (list)) '()
	(sort (list 1)) '(1)
	(sort (list "b" "a" "c")) '("a" "b" "c")
	(sort (list "b" "a" "b" "a")) '("a" "a" "b" "b")
	(sort (list 3 1 2) (# (- %0 %1))) '(1 2 3)
	(sort (list 3 1 2) (# (- %1 %0))) '(3 2 1)
	(sort (list 2 1 2 1 2) (# (- %0 %1))) '(1 1 2 2 2)
	(sort (list 1 1 1) (# (- %0 %1))) '(1 1 1)
	(sort (list 5 1 4 1 5 9 2 6 5 3 5) (# (- %0 %1))) '(1 1 2 3 4 5 5 5 5 6 9)
	;just a range of the list
	(sort (list 5 4 3 2 1) (const -) 1 4) '(5 2 3 4 1)
	(usort (list 3 1 3 2 1) (# (- %0 %1))) '(1 2 3)
	(usort (list)) '()
	(pivot (# (- %0 %1)) (list 1) 0 1) 0
	(shuffle (list)) '()
	(shuffle (list 1)) '(1))

;a compare function that throws stops the sort, and the default compare
;is for strings, so throws when given numbers
(assert-error "sort compare throws" (sort (list 3 1 2) (lambda (a b) (throw "e" 1))))
(assert-error "sort numbers with the string compare" (sort (list 3 1 2)))
(assert-error "usort numbers with the string compare" (usort (list 3 1 2)))
