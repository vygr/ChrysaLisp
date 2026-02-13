(report-header "Math & Logic")

; Arithmetic
(assert-eq "Add ints" 5 (+ 2 3))
(assert-eq "Sub ints" 5 (- 10 5))
(assert-eq "Mul ints" 20 (* 4 5))
(assert-eq "Div ints" 5 (/ 10 2))
(assert-eq "Modulus"  1 (% 10 3))
(assert-eq "Negate"  -5 (neg 5))
(assert-eq "Abs val"  5 (abs -5))
(assert-eq "Min"	  2 (min 10 2 30))
(assert-eq "Max"	 30 (max 10 2 30))

; Bitwise
(assert-eq "Logand"   1 (logand 3 1))
(assert-eq "Logior"   3 (logior 1 2))
(assert-eq "Logxor"   3 (logxor 1 2))
(assert-eq "Shift L"  4 (<< 1 2))
(assert-eq "Shift R"  2 (>> 8 2))

; Comparison
(assert-true "Equal ="   (= 10 10))
(assert-true "Not Eq /=" (/= 10 5))
(assert-true "Less <"	(< 5 10))
(assert-true "Greater >" (> 10 5))
(assert-true "LTE <="	(<= 5 5))
(assert-true "GTE >="	(>= 5 5))

; Logic
(assert-true "AND logic" (and :t :t))
(assert-eq   "AND short" :nil (and :nil (throw "Should not eval" :nil)))
(assert-true "OR logic"  (or :nil :t))

; --- num-intern ---
(assert-eq "num-intern" 42 (num-intern 42))

; --- Shift Operators ---
; >> is SHR (Logical Shift Right) -> Zero fill
; >>> is ASR (Arithmetic Shift Right) -> Sign extension
(assert-eq ">> logical" 9223372036854775804 (>> -8 1))
(assert-eq ">>> arithmetic" -4 (>>> -8 1))
(assert-eq "<< shift" 16 (<< 8 1))

(assert-eq "n2i" 5 (n2i (n2r 5.7)))
(defq rnd (random 100))
(assert-true "random" (and (>= rnd 0) (< rnd 100)))
