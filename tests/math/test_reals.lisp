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
(assert-eq "Real Pow"   (n2r 8.0) (pow (n2r 2.0) (n2r 3.0)))
(assert-eq "Real Pow root" (n2r 2.0) (pow (n2r 4.0) (n2r 0.5)))
(assert-eq "Real Pow less than 0" (n2r 0.5) (pow (n2r 2.0) (n2r -1.0)))
(assert-eq "Real Pow of 0" (n2r 0.0) (pow (n2r 0.0) (n2r 2.0)))
(assert-error "Real Pow of another type" (pow (n2r 2.0) 3))

; Exp and Log, each within a bit or so of the nearest double
(defq e_near (lambda (a b) (<= (abs (- a b)) (* (abs b) (str-to-real "4e-16")))))
(assert-eq "Real Exp 0" (n2r 1.0) (exp (n2r 0.0)))
(assert-true "Real Exp 1" (e_near (exp (n2r 1.0)) (str-to-real "2.718281828459045")))
(assert-true "Real Exp -10" (e_near (exp (n2r -10.0)) (str-to-real "4.5399929762484854e-05")))
(assert-true "Real Exp 700" (e_near (log (exp (n2r 700.0))) (n2r 700.0)))
(assert-true "Real Exp above range" (> (exp (n2r 710.0)) (str-to-real "1e308")))
(assert-eq "Real Exp below range" (n2r 0.0) (exp (n2r -750.0)))
(assert-eq "Real Log 1" (n2r 0.0) (log (n2r 1.0)))
(assert-eq "Real Log e" (n2r 1.0) (log (str-to-real "2.718281828459045")))
(assert-true "Real Log 10" (e_near (log (n2r 10.0)) (str-to-real "2.302585092994046")))
(assert-true "Real Log 1e-300" (e_near (log (str-to-real "1e-300")) (str-to-real "-690.7755278982137")))
(assert-true "Real Log subnormal" (e_near (log (str-to-real "5e-324")) (str-to-real "-744.4400719213812")))
(assert-true "Real Log of Exp" (e_near (log (exp (n2r 5.5))) (n2r 5.5)))
(assert-true "Real Log 0" (< (log (n2r 0.0)) (str-to-real "-1e308")))
(assert-error "Real Exp of another type" (exp 3))

; str-to-real, the nearest double, by its bits
(defq s2r_bits (lambda (s) (get-long (set-real (str-alloc 8) 0 (str-to-real s)) 0)))
(each (lambda ((s want)) (assert-eq (cat "Real from " s) want (s2r_bits s)))
	'(("0.1" 0x3FB999999999999A) ("2.718281828459045" 0x4005BF0A8B145769)
	("4.5399929762484854e-05" 0x3F07CD79B5647C9B) ("1e-300" 0x01A56E1FC2F8F359)
	("1.0142320547350045e304" 0x7F0D945DF4F8EC8E) ("1e308" 0x7FE1CCF385EBC8A0)
	("1.7976931348623157e308" 0x7FEFFFFFFFFFFFFF) ("2.2250738585072011e-308" 0x000FFFFFFFFFFFFF)
	("5e-324" 0x1) ("2.4703282292062327e-324" 0x0) ("2.4703282292062328e-324" 0x1)
	("9007199254740993" 0x4340000000000000) ("1e23" 0x44B52D02C7E14AF6)
	("3.141592653589793238462643383279" 0x400921FB54442D18) ("-0.5" 0xBFE0000000000000)
	("1e400" 0x7FF0000000000000) ("1e-400" 0x0)))
(assert-true "Fixed Exp and Log" (and (< (abs (- (exp 1.0) 2.71828)) 0.0002) (< (abs (- (log 2.0) 0.69315)) 0.0002)))

; Trig (Scalar)
(assert-eq "Real Sin 0" 0.0 (n2f (sin (n2r 0.0))))
(assert-eq "Real Cos 0" 1.0 (n2f (cos (n2r 0.0))))
; pi/2 is approx 1.57079632679
(assert-eq "Real Sin PI/2" 1.0 (n2f (sin (n2r 1.57079632679))))
(assert-eq "Real Cos PI/2" 0.0 (n2f (cos (n2r 1.57079632679))))

; Quantization is round(v / tol) * tol
(assert-eq "Real Quant" (n2r 1.0) (quant (n2r 1.2) (n2r 1.0)))
(assert-eq "Real Quant 2" (n2r 3.0) (quant (n2r 2.9) (n2r 0.5)))

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
(assert-eq "Reals Quant" (reals (n2r 1.0) (n2r 1.0) (n2r 2.0) (n2r -0.5)) (reals-quant rv_q (n2r 0.5)))

; verify types
