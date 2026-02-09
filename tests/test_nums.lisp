;;;;;;;;;;;;;;;;;;;;;;
; tests/test_nums.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "Nums Vectors (Integer)")

(defq n1 (nums 1 2 3))
(defq n2 (nums 4 5 6))
(defq n3 (nums -1 -2 -3))

; Basic Arithmetic
(assert-list-eq "Nums Add" '(5 7 9) (nums-add n1 n2))
(assert-list-eq "Nums Sub" '(-3 -3 -3) (nums-sub n1 n2))
(assert-list-eq "Nums Mul" '(4 10 18) (nums-mul n1 n2))
(assert-list-eq "Nums Div" '(0 0 0) (nums-div n1 n2))

; Vector Operations
(assert-eq "Nums Dot" 32 (nums-dot n1 n2))
(assert-list-eq "Nums Abs" '(1 2 3) (nums-abs n3))
(assert-eq "Nums Sum" 6 (nums-sum n1))

; verify types
(assert-true "Is nums?" (nums? n1))
