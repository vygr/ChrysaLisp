;;;;;;;;;;;;;;;;;;;;;;
; tests/test_reals.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "Reals (Floating Point)")

; Scalar Arithmetic
(assert-eq "Real Add" (n2r 3.0) (+ (n2r 1.0) (n2r 2.0)))
(assert-eq "Real Sub" (n2r -1.0) (- (n2r 1.0) (n2r 2.0)))
(assert-eq "Real Mul" (n2r 6.0) (* (n2r 2.0) (n2r 3.0)))
(assert-eq "Real Div" (n2r 0.5) (/ (n2r 1.0) (n2r 2.0)))
; 5.3 % 2.5 = 0.3
(assert-eq "Real Mod" (n2r 0.3) (% (n2r 5.3) (n2r 2.5)))

; Scalar Min/Max
(assert-eq "Real Min" (n2r 3.0) (min (n2r 3.0) (n2r 5.0)))
(assert-eq "Real Max" (n2r 5.0) (max (n2r 3.0) (n2r 5.0)))

; Unary Operations
(assert-eq "Real Recip" (n2r 2.0) (recip (n2r 0.5)))
(assert-eq "Real Abs"   (n2r 3.5) (abs (n2r -3.5)))
(assert-eq "Real Neg"   (n2r -4.0) (neg (n2r 4.0)))
(assert-eq "Real Floor" (n2r 3.0) (floor (n2r 3.7)))
(assert-eq "Real Ceil"  (n2r 4.0) (ceil (n2r 3.7)))
(assert-eq "Real Frac"  (n2r 0.75) (frac (n2r 3.75)))
(assert-eq "Real Sign"  (n2r 1.0) (sign (n2r 5.0)))
(assert-eq "Real Sqrt"  (n2r 3.0) (sqrt (n2r 9.0)))

; Trig (Scalar)
(assert-eq "Real Sin 0" 0.0 (n2f (sin (n2r 0.0))))
(assert-eq "Real Cos 0" 1.0 (n2f (cos (n2r 0.0))))
; pi/2 is approx 1.57079632679
(assert-eq "Real Sin PI/2" 1.0 (n2f (sin (n2r 1.57079632679))))
(assert-eq "Real Cos PI/2" 0.0 (n2f (cos (n2r 1.57079632679))))

; Quantization in ChrysaLisp seems to be (floor(v/tol) + 0.5) * tol
(assert-eq "Real Quant" (n2r 1.5) (quant (n2r 1.2) (n2r 1.0)))
(assert-eq "Real Quant 2" (n2r 2.75) (quant (n2r 2.9) (n2r 0.5)))

(report-header "Reals Vectors")

(defq rv1 (reals (n2r 1.0) (n2r 2.0) (n2r 3.0)))
(defq rv2 (reals (n2r 4.0) (n2r 8.0) (n2r 16.0)))
(defq rv3 (reals (n2r -1.0) (n2r -2.0) (n2r -3.0)))

(assert-eq "Reals Add" (reals (n2r 5.0) (n2r 10.0) (n2r 19.0)) (nums-add rv1 rv2))
(assert-eq "Reals Sub" (reals (n2r -3.0) (n2r -6.0) (n2r -13.0)) (nums-sub rv1 rv2))
(assert-eq "Reals Mul" (reals (n2r 4.0) (n2r 16.0) (n2r 48.0)) (nums-mul rv1 rv2))
; Use exact powers of 2 for division to avoid any precision issues
(assert-eq "Reals Div" (reals (n2r 0.25) (n2r 0.25) (n2r 0.1875)) (nums-div rv1 rv2))
(assert-eq "Reals Dot" (n2r 68.0) (nums-dot rv1 rv2))
(assert-eq "Reals Scale" (reals (n2r 2.0) (n2r 4.0) (n2r 6.0)) (nums-scale rv1 (n2r 2.0)))

(assert-eq "Reals Max" (reals (n2r 4.0) (n2r 8.0) (n2r 16.0)) (nums-max rv1 rv2))
(assert-eq "Reals Min" (reals (n2r 1.0) (n2r 2.0) (n2r 3.0)) (nums-min rv1 rv2))
(assert-eq "Reals Abs" (reals (n2r 1.0) (n2r 2.0) (n2r 3.0)) (nums-abs rv3))
(assert-eq "Reals Sum" (n2r 6.0) (nums-sum rv1))

(defq rv_fp (reals (n2r 1.5) (n2r -2.5) (n2r 3.75)))
(assert-eq "Reals Floor" (reals (n2r 1.0) (n2r -3.0) (n2r 3.0)) (fixeds-floor rv_fp))
(assert-eq "Reals Ceil"  (reals (n2r 2.0) (n2r -2.0) (n2r 4.0)) (fixeds-ceil rv_fp))
(assert-eq "Reals Frac"  (reals (n2r 0.5) (n2r 0.5) (n2r 0.75)) (fixeds-frac rv_fp))

(defq rv_q (reals (n2r 1.2) (n2r 0.8) (n2r 2.1) (n2r -0.6)))
(assert-eq "Reals Quant" (reals (n2r 1.25) (n2r 0.75) (n2r 2.25) (n2r -0.75)) (reals-quant rv_q (n2r 0.5)))

; verify types
(assert-true "Is reals?" (reals? rv1))
(assert-true "Is real?" (real? (n2r 1.0)))
