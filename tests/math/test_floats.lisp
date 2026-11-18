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
;the maths in 32 bits, by the f_ functions of sys/math: sin and cos the
;nearest single, where they come to 1 too, exp and log within a bit of it
(assert-true "float sin" (< (abs (- (n2d (sin (n2f 1))) (sin (n2d 1)))) (str-to-double "1e-7")))
(assert-eq "float cos of 0 is 1" (n2f 1) (cos (n2f 0)))
(assert-eq "float sin of 3pi/2 is -1" (n2f -1) (sin (n2f (str-to-double "4.71238898"))))
(assert-true "float cos 2" (< (abs (- (n2d (cos (n2f 2))) (str-to-double "-0.4161468365471424"))) (str-to-double "3e-8")))
(assert-true "float exp" (< (abs (- (n2d (exp (n2f 1))) (exp (n2d 1)))) (str-to-double "1e-6")))
(assert-true "float exp 3" (< (abs (- (n2d (exp (n2f 3))) (str-to-double "20.085536923187668"))) (str-to-double "4e-6")))
(assert-eq "float log of one" (n2f 0) (log (n2f 1)))
(assert-true "float log 10" (< (abs (- (n2d (log (n2f 10))) (str-to-double "2.302585092994046"))) (str-to-double "5e-7")))
(assert-true "float log of a subnormal" (< (abs (- (n2d (log (n2f (str-to-double "1e-40")))) (str-to-double "-92.10340910966488"))) (str-to-double "2e-5")))
(assert-true "float log of 0, minus infinity" (< (log (n2f 0)) (n2f (str-to-double "-3e38"))))
;tanh, by f_tanh in 32 bits, within two of its last bits of the nearest single
(defq tf_near (lambda (a b) (< (abs (- (n2d a) b)) (* (abs b) (str-to-double "2.5e-7")))))
(assert-eq "float tanh of 0" (n2f 0) (tanh (n2f 0)))
(assert-true "float tanh 0.25" (tf_near (tanh (n2f 0.25)) (str-to-double "0.24491866240370913")))
(assert-true "float tanh 0.5" (tf_near (tanh (n2f 0.5)) (str-to-double "0.46211715726000974")))
(assert-true "float tanh -2" (tf_near (tanh (n2f -2.0)) (str-to-double "-0.9640275800758169")))
(assert-eq "float tanh 20" (n2f 1) (tanh (n2f 20.0)))
(assert-eq "float tanh -20" (n2f -1) (tanh (n2f -20.0)))

(assert-error "float divide by zero" (/ tf_a (n2f 0)))
(assert-error "float mod by zero" (% tf_a (n2f 0)))

;compares across the sign, where the order of the raw bits is not the
;order of the floats
(defq tf_m2 (n2f -2) tf_m1 (n2f -1) tf_p1 (n2f 1))
(assert-true "float < negatives" (< tf_m2 tf_m1))
(assert-true "float < across zero" (< tf_m1 tf_p1))
(assert-true "float > negatives" (> tf_m1 tf_m2))
(assert-eq "float min negatives" tf_m2 (min tf_m1 tf_m2 tf_p1))
(assert-eq "float max negatives" tf_p1 (max tf_m1 tf_p1 tf_m2))
(assert-eq "float floor negative" (n2f -3) (floor (n2f -2.5)))
(assert-eq "float ceil negative" (n2f -2) (ceil (n2f -2.5)))

; --- floats, a vector of them, packed 4 bytes an element ---
(defun tf-floats (&rest ns) (apply (const floats) (map (const n2f) ns)))
(defun tf-ints (fs) (map (const n2i) fs))
(defq tf_v (tf-floats 1 2 3 4 5) tf_w (tf-floats 10 20 30 40 50))

(assert-eq "floats length" 5 (length tf_v))
(assert-eq "floats elem" (n2f 3) (elem-get tf_v 2))
(assert-true "floats elem is a float" (float? (elem-get tf_v 0)))
(assert-list-eq "floats slice" '(2 3 4) (tf-ints (slice tf_v 1 4)))
(assert-list-eq "floats slice reversed" '(5 4 3 2 1) (tf-ints (slice tf_v -1 0)))
(assert-list-eq "floats cat" '(1 2 3 4 5 10 20 30 40 50) (tf-ints (cat tf_v tf_w)))
(assert-eq "floats find" 2 (find (n2f 3) tf_v))
(assert-eq "floats find none" :nil (find (n2f 9) tf_v))
(assert-eq "floats rfind, the end of a slice" 3 (rfind (n2f 3) tf_v))
(assert-true "floats eql" (eql tf_v (cat tf_v)))
(assert-true "floats not eql" (not (eql tf_v tf_w)))
(assert-eq "floats hash" (hash tf_v) (hash (cat tf_v)))
(assert-list-eq "floats map" '(1 2 3 4 5) (map (const n2i) tf_v))

;elem-set, fill, push and pop, growing past its short storage
(defq tf_u (cat tf_v))
(elem-set tf_u 0 (n2f -7))
(assert-list-eq "floats elem-set" '(-7 2 3 4 5) (tf-ints tf_u))
(assert-list-eq "floats fill" '(9 9 9) (tf-ints (fill (floats) 3 (n2f 9))))
(defq tf_p (floats))
(push tf_p (n2f 1) (tf-floats 2 3))
(assert-list-eq "floats push of a float and a floats" '(1 2 3) (tf-ints tf_p))
(times 40 (push tf_p (n2f 4)))
(assert-eq "floats grown" 43 (length tf_p))
(assert-eq "floats pop" (n2f 4) (pop tf_p))
(assert-eq "floats popped" 42 (length tf_p))
;raw, as push is for the other arrays: a number's low 32 bits, an
;array's or a str's bytes 4 to an element
(defq tf_r (floats))
(push tf_r (n2f 2) "\x00\x00\x80\x3F" (nums 0x3F80000040000000))
(assert-list-eq "floats push raw" '(2 1 2 1) (tf-ints tf_r))

;the vector ops in 32 bits
(test-cases
	(tf-ints (nums-add tf_v tf_w)) '(11 22 33 44 55)
	(tf-ints (nums-sub tf_w tf_v)) '(9 18 27 36 45)
	(tf-ints (nums-mul tf_v tf_w)) '(10 40 90 160 250)
	(tf-ints (nums-div tf_w tf_v)) '(10 10 10 10 10)
	(tf-ints (nums-scale tf_v (n2f 2))) '(2 4 6 8 10)
	(tf-ints (nums-add-scaled tf_v tf_w (n2f 2))) '(21 42 63 84 105)
	(tf-ints (nums-min (tf-floats -1 5) (tf-floats 2 -3))) '(-1 -3)
	(tf-ints (nums-max (tf-floats -1 5) (tf-floats 2 -3))) '(2 5)
	(tf-ints (nums-abs (tf-floats -1 2))) '(1 2)
	(tf-ints (fixeds-floor (tf-floats -1.5 2.5))) '(-2 2)
	(tf-ints (fixeds-ceil (tf-floats -1.5 2.5))) '(-1 3)
	(n2d (elem-get (fixeds-frac (tf-floats -1.5 2.25)) 0)) (n2d 0.5)
	(nums-sum tf_v) (n2f 15)
	(nums-dot tf_v tf_w) (n2f 550))
(assert-error "floats divide by zero" (nums-div tf_v (tf-floats 1 0 1 1 1)))

;every vector of an op is of the one kind, a scaled one too: a floats'
;numbers are half the size of the others', so one among doubles would be
;read, or written, past its end. The floats written to has room for them
(defq tf_d (doubles (n2d 1) (n2d 2) (n2d 3) (n2d 4) (n2d 5)) tf_room (cap 16 (cat tf_v)))
(assert-error "doubles and floats added, scaled" (nums-add-scaled tf_d tf_v (n2d 1)))
(assert-error "floats and doubles added, scaled" (nums-add-scaled tf_v tf_d (n2f 1)))
(assert-error "doubles added, scaled, into floats" (nums-add-scaled tf_d tf_d (n2d 1) tf_room))
(assert-error "doubles scaled into floats" (nums-scale tf_d (n2d 2) tf_room))
(assert-error "floats scaled into doubles" (nums-scale tf_v (n2f 2) (cat tf_d)))
(assert-list-eq "and the floats are as they were" '(1 2 3 4 5) (tf-ints tf_room))

;into a buf whole, 4 bytes an element
(defq tf_b (buf 64))
(buf-set tf_b 0 20 tf_v)
(assert-eq "floats into a buf, 4 bytes each" 0x40000000 (code (buf-get tf_b 0 20) 4 4))
