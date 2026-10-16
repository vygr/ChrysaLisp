(report-header "Printing what holds itself: ... in place of going round for ever")

;A list that holds itself, or holds what holds it, was printed for ever,
;till the stack ran out and the node ended, an error with such a thing in
;its message too. The lists a task is printing are kept in its tcb, and
;one being printed already is ..., Chris: "We should still have :print
;detect cycles !". A list held twice, and not in itself, is printed in full
;each time: only a way round is cut

(defq pc_self (list 1 2))
(push pc_self pc_self)
(assert-eq "a list that holds itself" "(1 2 ...)" (str pc_self))

(defq pc_a (list :a) pc_b (list :b pc_a))
(push pc_a pc_b)
(assert-eq "two that hold each other" "(:a (:b ...))" (str pc_a))

(defq pc_shared (list 7 8))
(assert-eq "one held twice, not in itself, is printed each time" "((7 8) (7 8))" (str (list pc_shared pc_shared)))

(defq pc_env (env 1))
(def pc_env :me pc_env)
(assert-eq "an env that holds itself" "(:me ...)" (str pc_env))

(defq pc_q (list 'quote 1))
(elem-set pc_q 1 pc_q)
(assert-true "a quote of itself ends" (> (length (str pc_q)) 0))

(assert-eq "and the lists printed after are printed as ever" "(1 (2 (3)))" (str '(1 (2 (3)))))
;what holds what cannot be let go of by the count of who holds it: let go
;of here, round, or it leaks
(pop pc_self) (pop pc_a) (undef pc_env :me) (elem-set pc_q 1 1)
