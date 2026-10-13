;what the evaluator does all day: a form evaluated, its args made, used,
;and let go of, and a list made and dropped
(defq be_n 2000000 be_three (list 1 2 3) be_eight (list 1 2 3 4 5 6 7 8))
(defun be-f (a b) a)

(bench "empty loop" be_n (times be_n :nil))
(bench "(list 1)" be_n (times be_n (list 1)))
(bench "(list 1 2 3)" be_n (times be_n (list 1 2 3)))
(bench "(+ 1 2)" be_n (times be_n (+ 1 2)))
(bench "(eql 1 2)" be_n (times be_n (eql 1 2)))
(bench "(be-f 1 2), a lambda" be_n (times be_n (be-f 1 2)))
(bench "(slice three 0 -1)" be_n (times be_n (slice be_three 0 -1)))
(bench "(cat three three)" be_n (times be_n (cat be_three be_three)))
(bench "(slice eight 0 -1)" be_n (times be_n (slice be_eight 0 -1)))
(bench "(list (list 1) 2)" be_n (times be_n (list (list 1) 2)))
