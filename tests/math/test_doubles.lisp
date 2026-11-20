(report-header "Doubles (Floating Point)")

; Scalar Arithmetic
(assert-eq "Double Add" (n2d 3.0) (+ (n2d 1.0) (n2d 2.0)))
(assert-eq "Double Sub" (n2d -1.0) (- (n2d 1.0) (n2d 2.0)))
(assert-eq "Double Mul" (n2d 6.0) (* (n2d 2.0) (n2d 3.0)))
(assert-eq "Double Div" (n2d 0.5) (/ (n2d 1.0) (n2d 2.0)))
; 5.3 % 2.5 = 0.3
(assert-eq "Double Mod" (n2d 0.3) (% (n2d 5.3) (n2d 2.5)))

; Scalar Min/Max
(assert-eq "Double Min" (n2d 3.0) (min (n2d 3.0) (n2d 5.0)))
(assert-eq "Double Max" (n2d 5.0) (max (n2d 3.0) (n2d 5.0)))

; Unary Operations
(assert-eq "Double Recip" (n2d 2.0) (recip (n2d 0.5)))
(assert-eq "Double Abs"   (n2d 3.5) (abs (n2d -3.5)))
(assert-eq "Double Neg"   (n2d -4.0) (neg (n2d 4.0)))
(assert-eq "Double Floor" (n2d 3.0) (floor (n2d 3.7)))
(assert-eq "Double Ceil"  (n2d 4.0) (ceil (n2d 3.7)))
(assert-eq "Double Frac"  (n2d 0.75) (frac (n2d 3.75)))
(assert-eq "Double Sign"  (n2d 1.0) (sign (n2d 5.0)))
(assert-eq "Double Sqrt"  (n2d 3.0) (sqrt (n2d 9.0)))
(assert-eq "Double Pow"   (n2d 8.0) (pow (n2d 2.0) (n2d 3.0)))
(assert-eq "Double Pow root" (n2d 2.0) (pow (n2d 4.0) (n2d 0.5)))
(assert-eq "Double Pow less than 0" (n2d 0.5) (pow (n2d 2.0) (n2d -1.0)))
(assert-eq "Double Pow of 0" (n2d 0.0) (pow (n2d 0.0) (n2d 2.0)))
(assert-error "Double Pow of another type" (pow (n2d 2.0) 3))

; Exp and Log, each within a bit or so of the nearest double
(defq e_near (lambda (a b) (<= (abs (- a b)) (* (abs b) (str-to-double "4e-16")))))
(assert-eq "Double Exp 0" (n2d 1.0) (exp (n2d 0.0)))
(assert-true "Double Exp 1" (e_near (exp (n2d 1.0)) (str-to-double "2.718281828459045")))
(assert-true "Double Exp -10" (e_near (exp (n2d -10.0)) (str-to-double "4.5399929762484854e-05")))
(assert-true "Double Exp 700" (e_near (log (exp (n2d 700.0))) (n2d 700.0)))
(assert-true "Double Exp above range" (> (exp (n2d 710.0)) (str-to-double "1e308")))
(assert-eq "Double Exp below range" (n2d 0.0) (exp (n2d -750.0)))
(assert-eq "Double Log 1" (n2d 0.0) (log (n2d 1.0)))
(assert-eq "Double Log e" (n2d 1.0) (log (str-to-double "2.718281828459045")))
(assert-true "Double Log 10" (e_near (log (n2d 10.0)) (str-to-double "2.302585092994046")))
(assert-true "Double Log 1e-300" (e_near (log (str-to-double "1e-300")) (str-to-double "-690.7755278982137")))
(assert-true "Double Log subnormal" (e_near (log (str-to-double "5e-324")) (str-to-double "-744.4400719213812")))
(assert-true "Double Log of Exp" (e_near (log (exp (n2d 5.5))) (n2d 5.5)))
(assert-true "Double Log 0" (< (log (n2d 0.0)) (str-to-double "-1e308")))
(assert-error "Double Exp of another type" (exp 3))

;the compares are the CPU's, as IEEE has them: -0.0 is = 0.0, though eql
;is of the bits and tells them apart, and a NaN is not =, <, <=, > nor >=
;anything, itself too, only /=
(defq dc_inf (exp (n2d 1000)) dc_nan (- dc_inf dc_inf) dc_z (n2d 0) dc_mz (neg dc_z) dc_one (n2d 1))
(assert-true "Double -0.0 = 0.0" (= dc_mz dc_z))
(assert-true "Double -0.0 not /= 0.0" (not (/= dc_mz dc_z)))
(assert-true "Double -0.0 not eql 0.0" (not (eql dc_mz dc_z)))
(assert-true "Double -0.0 <= and >= 0.0, not < nor >"
	(and (<= dc_mz dc_z) (>= dc_mz dc_z) (not (< dc_mz dc_z)) (not (> dc_mz dc_z))))
(assert-true "Double NaN not = itself" (not (= dc_nan dc_nan)))
(assert-true "Double NaN /= itself" (/= dc_nan dc_nan))
(assert-true "Double NaN eql itself" (eql dc_nan dc_nan))
(assert-true "Double NaN no order with 0"
	(not (or (< dc_nan dc_z) (<= dc_nan dc_z) (> dc_nan dc_z) (>= dc_nan dc_z)
		(< dc_z dc_nan) (<= dc_z dc_nan) (> dc_z dc_nan) (>= dc_z dc_nan))))
(assert-true "Double NaN in a chain" (not (< dc_z dc_nan dc_one)))
(assert-true "Double = chain" (and (= dc_one dc_one dc_one) (not (= dc_one dc_one dc_z))))
(assert-true "Double /= no two equal" (and (/= dc_z dc_one dc_inf) (not (/= dc_z dc_one dc_mz))))

;tanh, within two of its last bits of the true one, d_tanh: of each way it
;is worked out, the series below 0.55 and by exp above, 1 from 22 on, and
;odd
(defq t_near (lambda (a b) (<= (abs (- a b)) (* (abs b) (str-to-double "5e-16")))))
(assert-eq "Double Tanh 0" (n2d 0.0) (tanh (n2d 0.0)))
(assert-true "Double Tanh small" (t_near (tanh (str-to-double "1e-8")) (str-to-double "1e-8")))
(assert-true "Double Tanh 0.25" (t_near (tanh (str-to-double "0.25")) (str-to-double "0.24491866240370913")))
(assert-true "Double Tanh 0.5" (t_near (tanh (str-to-double "0.5")) (str-to-double "0.46211715726000974")))
(assert-true "Double Tanh 1" (t_near (tanh (n2d 1.0)) (str-to-double "0.7615941559557649")))
(assert-true "Double Tanh 3" (t_near (tanh (n2d 3.0)) (str-to-double "0.9950547536867305")))
(assert-eq "Double Tanh 30" (n2d 1.0) (tanh (n2d 30.0)))
(assert-true "Double Tanh -0.75" (t_near (tanh (str-to-double "-0.75")) (str-to-double "-0.6351489523872873")))
(assert-true "Double Tanh -2" (t_near (tanh (n2d -2.0)) (str-to-double "-0.9640275800758169")))
(assert-eq "Double Tanh -30" (n2d -1.0) (tanh (n2d -30.0)))

; random, a double from 0 to it, not only whole numbers
(defq dr_got (map! (lambda () (random (n2d 1))) '() 0 200))
(assert-true "random of a double is below it" (every (# (and (>= %0 (n2d 0)) (< %0 (n2d 1)))) dr_got))
(assert-true "and not only whole numbers" (some (# (> %0 (n2d 0))) dr_got))
(assert-true "spread over it" (and (some (# (< %0 (n2d 0.25))) dr_got) (some (# (> %0 (n2d 0.75))) dr_got)))

; str-to-double, the nearest double, by its bits
(defq s2r_bits (lambda (s) (get-long (set-double (str-alloc 8) 0 (str-to-double s)) 0)))
(each (lambda ((s want)) (assert-eq (cat "Double from " s) want (s2r_bits s)))
	'(("0.1" 0x3FB999999999999A) ("2.718281828459045" 0x4005BF0A8B145769)
	("4.5399929762484854e-05" 0x3F07CD79B5647C9B) ("1e-300" 0x01A56E1FC2F8F359)
	("1.0142320547350045e304" 0x7F0D945DF4F8EC8E) ("1e308" 0x7FE1CCF385EBC8A0)
	("1.7976931348623157e308" 0x7FEFFFFFFFFFFFFF) ("2.2250738585072011e-308" 0x000FFFFFFFFFFFFF)
	("5e-324" 0x1) ("2.4703282292062327e-324" 0x0) ("2.4703282292062328e-324" 0x1)
	("9007199254740993" 0x4340000000000000) ("1e23" 0x44B52D02C7E14AF6)
	("3.141592653589793238462643383279" 0x400921FB54442D18) ("-0.5" 0xBFE0000000000000)
	("1e400" 0x7FF0000000000000) ("1e-400" 0x0)))
(assert-error "Fixed Exp, of a double only" (exp 1.0))

; Trig (Scalar)
(assert-eq "Double Sin 0" 0.0 (n2q (sin (n2d 0.0))))
(assert-eq "Double Cos 0" 1.0 (n2q (cos (n2d 0.0))))
; pi/2 is approx 1.57079632679
(assert-eq "Double Sin PI/2" 1.0 (n2q (sin (n2d 1.57079632679))))
(assert-eq "Double Cos PI/2" 0.0 (n2q (cos (n2d 1.57079632679))))

; Quantization is round(v / tol) * tol
(assert-eq "Double Quant" (n2d 1.0) (quant (n2d 1.2) (n2d 1.0)))
(assert-eq "Double Quant 2" (n2d 3.0) (quant (n2d 2.9) (n2d 0.5)))

(report-header "Doubles Vectors")

(defq rv1 (doubles (n2d 1.0) (n2d 2.0) (n2d 3.0)))
(defq rv2 (doubles (n2d 4.0) (n2d 8.0) (n2d 16.0)))
(defq rv3 (doubles (n2d -1.0) (n2d -2.0) (n2d -3.0)))

(assert-eq "Doubles Add" (doubles (n2d 5.0) (n2d 10.0) (n2d 19.0)) (nums-add rv1 rv2))
(assert-eq "Doubles Sub" (doubles (n2d -3.0) (n2d -6.0) (n2d -13.0)) (nums-sub rv1 rv2))
(assert-eq "Doubles Mul" (doubles (n2d 4.0) (n2d 16.0) (n2d 48.0)) (nums-mul rv1 rv2))
; Use exact powers of 2 for division to avoid any precision issues
(assert-eq "Doubles Div" (doubles (n2d 0.25) (n2d 0.25) (n2d 0.1875)) (nums-div rv1 rv2))
(assert-eq "Doubles Scale" (doubles (n2d 2.0) (n2d 4.0) (n2d 6.0)) (nums-scale rv1 (n2d 2.0)))

(assert-eq "Doubles Max" (doubles (n2d 4.0) (n2d 8.0) (n2d 16.0)) (nums-max rv1 rv2))
(assert-eq "Doubles Min" (doubles (n2d 1.0) (n2d 2.0) (n2d 3.0)) (nums-min rv1 rv2))
(assert-eq "Doubles Abs" (doubles (n2d 1.0) (n2d 2.0) (n2d 3.0)) (nums-abs rv3))

(defq rv_fp (doubles (n2d 1.5) (n2d -2.5) (n2d 3.75)))
(assert-eq "Doubles Floor" (doubles (n2d 1.0) (n2d -3.0) (n2d 3.0)) (fixeds-floor rv_fp))
(assert-eq "Doubles Ceil"  (doubles (n2d 2.0) (n2d -2.0) (n2d 4.0)) (fixeds-ceil rv_fp))
(assert-eq "Doubles Frac"  (doubles (n2d 0.5) (n2d 0.5) (n2d 0.75)) (fixeds-frac rv_fp))

(defq rv_q (doubles (n2d 1.2) (n2d 0.8) (n2d 2.1) (n2d -0.6)))
(assert-eq "Doubles Quant" (doubles (n2d 1.0) (n2d 1.0) (n2d 2.0) (n2d -0.5)) (doubles-quant rv_q (n2d 0.5)))
