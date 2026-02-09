;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_multi_seq.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Multi-Sequence Ops: map, some, reduce")

; --- Map Multi ---
(defq m_res (map list '(a b c) (nums 1 2 3) "qwe"))
(assert-list-eq "map multi" '((a 1 "q") (b 2 "w") (c 3 "e")) m_res)

; --- Some Multi ---
; some returns the first non-nil result
(defq s_res (some (lambda (x y z) (if (eql x 'b) (list x y z))) 
                 '(a b c) (nums 1 2 3) "abc"))
(assert-list-eq "some multi" '(b 2 "b") s_res)

; every (uses some! with :t mode)
(assert-true "every multi" (every (lambda (x y) (eql (str x) (str y))) 
                                 '(1 2 3) (nums 1 2 3)))

; --- Reduce Multi ---
; Note: use reduce! for multi-sequence, as reduce macro wraps single sequence in list
(defq r_res (reduce! (lambda (acc x y) (push acc (list x y))) 
                    (list '(a b) (nums 10 20)) 
                    (list)))
(assert-list-eq "reduce multi" '((a 10) (b 20)) r_res)

; --- Length Mismatch ---
; Should stop at the shortest sequence
(defq short_res (map list '(a b c d e) (nums 1 2)))
(assert-list-eq "map multi shortest" '((a 1) (b 2)) short_res)

; --- Filter! ---
(defq f_res (filter! (lambda (x) (even? x)) (nums 1 2 3 4 5 6)))
(assert-list-eq "filter!" '(2 4 6) f_res)