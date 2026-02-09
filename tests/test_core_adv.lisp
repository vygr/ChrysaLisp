;;;;;;;;;;;;;;;;;;;;;;
; tests/test_core_adv.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "Advanced Core: catch, bind, apply, eval")

; --- catch & throw ---
(defq catch_res (catch 
    (throw "error message" 42)
    (progn 
        ; In catch handler, _ is the error string containing the object
        (assert-true "catch object in msg" (found? _ "42"))
        "recovered")))
(assert-eq "catch recovery" "recovered" catch_res)

; --- bind options ---
(defq b_val :nil)
(bind '(& b_val) '(10 20))
(assert-eq "bind & skip" 20 b_val)

(bind '(& &rest b_val) '(1 2 3))
(assert-list-eq "bind &rest" '(2 3) b_val)

(bind '(&most b_val &) '(1 2 3 4))
(assert-list-eq "bind &most" '(1 2 3) b_val)

(defq bo1 0 bo2 0)
(bind '(bo1 &optional bo2) '(100))
(assert-eq "bind &optional 1" 100 bo1)
(assert-eq "bind &optional 2" :nil bo2)

(defq bi1 0 bi2 0)
(bind '(bi1 &ignore bi2) '(1 2 3))
(assert-eq "bind &ignore 1" 1 bi1)
(assert-eq "bind &ignore 2" 0 bi2) ; bi2 remains 0, skipped

; --- bind destructuring ---
(defq dx 0 dy 0)
(bind '((dx dy)) (list (list 12 13)))
(assert-eq "bind destructuring dx" 12 dx)
(assert-eq "bind destructuring dy" 13 dy)

; --- apply ---
(assert-eq "apply +" 6 (apply (const +) '(1 2 3)))

; --- identity ---
(assert-eq "identity" 100 (identity 100))

; --- eval ---
(assert-eq "eval" 5 (eval '(+ 2 3)))

; --- macroexpand ---
; (inc x) expands to (#add x 1) in this environment
(defq exp (macroexpand '(inc x)))
(assert-eq "macroexpand first" "#add" (str (first exp)))
(assert-eq "macroexpand second" 'x (second exp))
(assert-eq "macroexpand third" 1 (third exp))

; --- eval-list ---
(assert-list-eq "eval-list" '(3 7) (eval-list '((+ 1 2) (+ 3 4))))

; --- prebind ---
(defq px 10)
(assert-list-eq "prebind" '(px 20) (prebind '(px 20)))

; --- read from sstream ---
(defq ss (string-stream "(a b c)"))
(defq r_res (read ss))
(assert-list-eq "read sstream" '(a b c) (first r_res))
