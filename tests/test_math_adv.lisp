;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_math_adv.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Advanced Math & Predicates")

; Numeric Predicates
(assert-true "neg? true"  (neg? -5))
(assert-true "neg? false" (not (neg? 5)))
(assert-true "pos? true"  (pos? 5))
(assert-true "pos? false" (not (pos? -5)))
(assert-true "odd? true"  (odd? 3))
(assert-true "odd? false" (not (odd? 4)))
(assert-true "even? true" (even? 10))
(assert-true "even? false" (not (even? 11)))

; Bitwise & Counting
(assert-eq "lognot" -1 (lognot 0))
(assert-eq "log2 8"  3 (log2 8))
(assert-eq "log2 16" 4 (log2 16))
(assert-eq "pow 2 3" 8 (pow 2 3))
(assert-eq "pow 3 2" 9 (pow 3 2))

; nlz, nlo, ntz, nto (Leading/Trailing Zeros/Ones)
; 0 is 000...000 (64 bits)
(assert-eq "nlz 0" 0 (nlz 0))
(assert-eq "ntz 0" 64 (ntz 0))
; 1 is 000...001
(assert-eq "ntz 1" 0 (ntz 1))
(assert-eq "nlz 1" 63 (nlz 1))
; -1 is 111...111
(assert-eq "nlo -1" 0 (nlo -1))
(assert-eq "nto -1" 64 (nto -1))

; Increment/Decrement Macros
(defq x 10)
(assert-eq "inc" 11 (inc x))
(assert-eq "dec" 9  (dec x))
(++ x)
(assert-eq "++" 11 x)
(-- x 2)
(assert-eq "--" 9 x)

; Logical Macros
(assert-true  "when"   (when :t :t))
(assert-true  "unless" (unless :nil :t))
(assert-eq    "or short" 5 (or :nil 5))
(assert-eq    "and short" 10 (and :t 10))
