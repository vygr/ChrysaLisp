(report-header "Floats (32 bit Floating Point)")

;a float holds the 32 bits of an IEEE single, the upper half of its value
;clear, so the same float is always the same value
(defq tf_a (n2f 1.5) tf_b (n2f (n2d 0.25)) tf_c (n2f 3) tf_m (n2f -2.5))

(test-cases
	;from each kind of number, the nearest float
	(n2d (n2f 1.5)) (n2d 1.5)
	(n2d (n2f 3)) (n2d 3)
	(n2d (n2f (n2d -2.5))) (n2d -2.5)
	;and back to each
	(n2i (n2f 4.75)) 4
	(n2q (n2f 1.5)) 1.5
	(n2q (n2f -2.5)) -2.5
	(n2d (n2f (n2d 0.25))) (n2d 0.25)
	(n2f tf_a) tf_a)

(test-cases
	;arithmetic in 32 bits
	(+ tf_a tf_b) (n2f 1.75)
	(- tf_a tf_b) (n2f 1.25)
	(* tf_a tf_c) (n2f 4.5)
	(/ tf_c (n2f 4)) (n2f 0.75)
	(% (n2f 7.5) (n2f 2)) (n2f 1.5)
	(+ tf_a tf_b tf_c) (n2f 4.75)
	(min tf_a tf_b tf_c) tf_b
	(max tf_a tf_b tf_c) tf_c
	(abs tf_m) (n2f 2.5)
	(neg tf_a) (n2f -1.5)
	(floor tf_m) (n2f -3)
	(ceil tf_m) (n2f -2)
	(frac tf_m) (n2f 0.5)
	(recip (n2f 4)) (n2f 0.25)
	(sqrt (n2f 16)) (n2f 4)
	(sign tf_m) (n2f -1))

;compares, chained as the others are
(assert-true "float <" (< tf_b tf_a tf_c))
(assert-true "float > not" (not (> tf_b tf_a)))
(assert-true "float <=" (<= tf_b tf_b tf_a))
(assert-true "float >=" (>= tf_c tf_a tf_a))
(assert-true "float = negative" (= tf_m (n2f -2.5)))
(assert-true "float /=" (/= tf_a tf_b))

;rounded to 32 bits: a third is not a double's third, but the float nearest it
(assert-true "a float third" (/= (n2d (/ (n2f 1) (n2f 3))) (/ (n2d 1) (n2d 3))))
(assert-true "a float third, its 24 bits" (< (abs (- (n2d (/ (n2f 1) (n2f 3))) (/ (n2d 1) (n2d 3))))
	(str-to-double "1e-7")))
;the maths by the double routines, rounded
(assert-true "float sin" (< (abs (- (n2d (sin (n2f 1))) (sin (n2d 1)))) (str-to-double "1e-7")))
(assert-true "float exp" (< (abs (- (n2d (exp (n2f 1))) (exp (n2d 1)))) (str-to-double "1e-6")))
(assert-eq "float log of one" (n2f 0) (log (n2f 1)))

(assert-error "float divide by zero" (/ tf_a (n2f 0)))
(assert-error "float mod by zero" (% tf_a (n2f 0)))
