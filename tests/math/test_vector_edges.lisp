(report-header "Vector, Fixed & Double Edges: signs, rounding, in place results")

; --- nums ---
(test-cases
	(nums-add (nums 1 2) (nums 3 4)) (nums 4 6)
	;wraps, as plain integer add does
	(nums-add (nums 9223372036854775807) (nums 1)) (nums -9223372036854775808)
	(nums-sub (nums 1 2) (nums 3 4)) (nums -2 -2)
	(nums-mul (nums -1 2) (nums 3 -4)) (nums -3 -8)
	(nums-div (nums 7 -7) (nums 2 2)) (nums 3 -3)
	(nums-mod (nums 7 -7) (nums 2 2)) (nums 1 -1)
	(nums-min (nums 1 5) (nums 3 4)) (nums 1 4)
	(nums-max (nums 1 5) (nums 3 4)) (nums 3 5)
	(nums-abs (nums -1 0 1)) (nums 1 0 1)
	(nums-sum (nums 5)) 5
	(nums-dot (nums 1 2) (nums 3 4)) 11
	(nums-scale (nums 1 2) 3) (nums 3 6)
	(nums-scale (nums 1 2) 0) (nums 0 0)
	(length (nums)) 0)

;the result can go to a given vector, even one of the inputs
(defq ve_a (nums 1 2) ve_b (nums 3 4) ve_out (nums 0 0))
(nums-add ve_a ve_b ve_out)
(assert-true "nums-add into output" (eql ve_out (nums 4 6)))
(nums-add ve_a ve_a ve_a)
(assert-true "nums-add in place" (eql ve_a (nums 2 4)))

(assert-true "nums? nums" (nums? (nums)))
(assert-true "nums? list" (not (nums? (list))))
(assert-true "fixeds? fixeds" (fixeds? (fixeds 1.0)))
(assert-true "fixeds? nums" (not (fixeds? (nums 1))))
(assert-true "doubles? doubles" (doubles? (doubles (n2d 1))))

;an empty vector has no sum or dot, and can not be added
(assert-error "nums-sum empty" (nums-sum (nums)))
(assert-error "nums-sum empty fixeds" (nums-sum (fixeds)))
(assert-error "nums-dot empty" (nums-dot (nums) (nums)))
(assert-error "nums-add empty" (nums-add (nums) (nums)))

; --- fixeds ---
(test-cases
	(nums-add (fixeds 1.5) (fixeds 2.25)) (fixeds 3.75)
	(nums-mul (fixeds 1.5) (fixeds 2.0)) (fixeds 3.0)
	(nums-div (fixeds 1.0) (fixeds 4.0)) (fixeds 0.25)
	(nums-sum (fixeds 1.5 2.5)) 4.0
	(fixeds-floor (fixeds 1.5 -1.5)) (fixeds 1.0 -2.0)
	(fixeds-ceil (fixeds 1.5 -1.5)) (fixeds 2.0 -1.0)
	;the fraction is what is left above the floor, so is never negative
	(fixeds-frac (fixeds 1.5 -1.5)) (fixeds 0.5 0.5))

; --- fixed point scalars ---
(test-cases
	(+ 1.5 2.25) 3.75		(- 1.5 2.25) -0.75
	(* 1.5 2.0) 3.0
	(/ 1.0 4.0) 0.25		(/ -1.0 4.0) -0.25
	;as with integers, the remainder takes the sign of the dividend
	(% 5.5 2.0) 1.5			(% -5.5 2.0) -1.5
	(neg 1.5) -1.5			(abs -1.5) 1.5
	(floor 1.5) 1.0			(floor -1.5) -2.0
	(ceil 1.5) 2.0			(ceil -1.5) -1.0
	(frac 1.75) 0.75		(frac -1.75) 0.25
	(floor 2.0) 2.0			(ceil 2.0) 2.0			(frac 2.0) 0.0
	(sqrt 4.0) 2.0			(sqrt 0.0) 0.0
	(sign -1.5) -1.0		(sign 0.0) 0.0
	(min 1.5 -2.5) -2.5		(max 1.5 -2.5) 1.5
	(= 1.0 1.0) :t			(< 1.0 1.5) :t
	(str 1.5) "1.50000"		(str -0.25) "-0.25000")

; --- conversions ---
(test-cases
	(n2i 1.0) 1				(n2i 5.9) 5
	(n2q 2) 2.0				(n2q -2) -2.0
	(n2i (n2q 7)) 7
	(n2q (n2d 1.5)) 1.5
	(n2i (n2d 1.5)) 1
	;n2i is the fastest conversion for each type, with no rounding mode, so
	;for a negative value with a fraction a fixed goes down, a double toward zero
	(n2i -5.9) -6
	(n2i (n2d -5.9)) -5
	;use floor or ceil first when the direction matters
	(n2i (floor -5.9)) -6
	(n2i (floor (n2d -5.9))) -6
	(n2i (ceil -5.9)) -5
	(n2i (ceil (n2d -5.9))) -5
	(n2i -5.0) -5
	(n2i (n2d -5.0)) -5
	(type-of 1.0) '(:num :fixed)
	(type-of (n2d 1)) '(:num :fixed :double))

(assert-true "fixed? fixed" (fixed? 1.0))
(assert-true "fixed? num" (not (fixed? 1)))
(assert-true "double? double" (double? (n2d 1)))
(assert-true "double? fixed" (not (double? 1.0)))
(assert-true "num? double" (num? (n2d 1)))

; --- doubles ---
(test-cases
	(n2i (+ (n2d 1) (n2d 2))) 3
	(n2i (* (n2d 3) (n2d 4))) 12
	(n2q (/ (n2d 1) (n2d 4))) 0.25
	(n2i (neg (n2d 5))) -5
	(n2q (abs (n2d -1.5))) 1.5
	(n2q (sqrt (n2d 4))) 2.0
	(n2q (floor (n2d -1.5))) -2.0
	(n2q (ceil (n2d -1.5))) -1.0
	(n2q (frac (n2d 1.75))) 0.75
	(n2q (recip (n2d 4))) 0.25
	(< (n2d 1) (n2d 2)) :t
	(= (n2d 1) (n2d 1)) :t
	(eql (n2d 1.5) (n2d 1.5)) :t
	(double-to-str (n2d 1.5)) "1.5"
	(n2q (str-to-double "1.5")) 1.5
	(n2q (str-to-double "-0.25")) -0.25
	(n2q (str-to-double "1e2")) 100.0
	(n2q (str-to-double "0")) 0.0
	(n2q (str-to-double "")) 0.0
	(n2q (sin 0.0)) 0.0
	(n2q (cos 0.0)) 1.0)
