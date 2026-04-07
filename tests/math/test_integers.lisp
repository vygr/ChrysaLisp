(report-header "Integers: arithmetic, bitwise, comparison, counting, shifts")

; --- Arithmetic ---
(assert-eq "Add ints" 5 (+ 2 3))
(assert-eq "Sub ints" 5 (- 10 5))
(assert-eq "Mul ints" 20 (* 4 5))
(assert-eq "Div ints" 5 (/ 10 2))
(assert-eq "Modulus"  1 (% 10 3))
(assert-eq "Negate"  -5 (neg 5))
(assert-eq "Abs val"  5 (abs -5))
(assert-eq "Min"	  2 (min 10 2 30))
(assert-eq "Max"	 30 (max 10 2 30))

; --- Comparison ---
(assert-true "Equal ="   (= 10 10))
(assert-true "Not Eq /=" (/= 10 5))
(assert-true "Less <"	(< 5 10))
(assert-true "Greater >" (> 10 5))
(assert-true "LTE <="	(<= 5 5))
(assert-true "GTE >="	(>= 5 5))

; --- Numeric Predicates ---
(assert-true "neg? true"  (neg? -5))
(assert-true "neg? false" (not (neg? 5)))
(assert-true "pos? true"  (pos? 5))
(assert-true "pos? false" (not (pos? -5)))
(assert-true "odd? true"  (odd? 3))
(assert-true "odd? false" (not (odd? 4)))
(assert-true "even? true" (even? 10))
(assert-true "even? false" (not (even? 11)))

; --- Bitwise ---
(assert-eq "Lognot" -1 (lognot 0))
(assert-eq "Logand"   1 (logand 3 1))
(assert-eq "Logior"   3 (logior 1 2))
(assert-eq "Logxor"   3 (logxor 1 2))

; --- Shift Operators ---
; >> is SHR (Logical Shift Right) -> Zero fill
; >>> is ASR (Arithmetic Shift Right) -> Sign extension
(assert-eq ">> logical" 9223372036854775804 (>> -8 1))
(assert-eq ">>> arithmetic" -4 (>>> -8 1))
(assert-eq "<< shift" 16 (<< 8 1))

; --- Bit Counting ---
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

; --- Math Ops ---
(assert-eq "log2 8"  3 (log2 8))
(assert-eq "log2 16" 4 (log2 16))
(assert-eq "pow 2 3" 8 (pow 2 3))
(assert-eq "pow 3 2" 9 (pow 3 2))

; --- Increment/Decrement Macros ---
(defq x 10)
(assert-eq "inc" 11 (inc x))
(assert-eq "dec" 9  (dec x))
(++ x)
(assert-eq "++" 11 x)
(-- x 2)
(assert-eq "--" 9 x)

; --- Misc ---
(assert-eq "num-intern" 42 (num-intern 42))
(assert-eq "n2i" 5 (n2i (n2r 5.7)))
(defq rnd (random 100))
(assert-true "random" (and (>= rnd 0) (< rnd 100)))
