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
	(nums-add-scaled (nums 1 2) (nums 3 4) 2) (nums 7 10)
	(nums-add-scaled (nums 1 2) (nums 3 4) -1) (nums -2 -2)
	(length (nums)) 0)

;the result can go to a given vector, even one of the inputs
(defq ve_a (nums 1 2) ve_b (nums 3 4) ve_out (nums 0 0))
(nums-add ve_a ve_b ve_out)
(assert-true "nums-add into output" (eql ve_out (nums 4 6)))
(nums-add ve_a ve_a ve_a)
(assert-true "nums-add in place" (eql ve_a (nums 2 4)))
(nums-add-scaled ve_a ve_b 3 ve_a)
(assert-true "nums-add-scaled in place" (eql ve_a (nums 11 16)))


;an empty vector has no sum or dot, and can not be added
(assert-error "nums-sum empty" (nums-sum (nums)))
(assert-error "nums-sum empty fixeds" (nums-sum (fixeds)))
(assert-error "nums-dot empty" (nums-dot (nums) (nums)))
(assert-error "nums-add empty" (nums-add (nums) (nums)))
(assert-error "nums-add-scaled empty" (nums-add-scaled (nums) (nums) 2))
(assert-error "nums-add-scaled second short" (nums-add-scaled (nums 1 2) (nums 1) 2))
(assert-error "nums-add-scaled output short" (nums-add-scaled (nums 1 2) (nums 1 2) 2 (nums 0)))

; --- fixeds ---
(test-cases
	(nums-add-scaled (fixeds 1.0 2.0) (fixeds 0.5 0.25) 2.0) (fixeds 2.0 2.5)
	(nums-add-scaled (fixeds 1.0) (fixeds 0.5) -1.5) (fixeds 0.25))

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

;the dot and sum by four running sums, a block of four, the rest, and none
;but the rest, whole numbers so the order of the adds can not show
(defq ve_d9 (apply (const doubles) (map! (# (n2d (inc (!)))) '() 0 9))
	ve_r9 (apply (const doubles) (map! (# (n2d (- 9 (!)))) '() 0 9)))
(test-cases
	(n2i (nums-sum ve_d9)) 45
	(n2i (nums-sum (slice ve_d9 0 4))) 10
	(n2i (nums-sum (slice ve_d9 0 3))) 6
	(n2i (nums-dot ve_d9 ve_r9)) 165
	(n2i (nums-dot (slice ve_d9 0 8) (slice ve_r9 0 8))) 156
	(n2i (nums-dot (slice ve_d9 0 1) (slice ve_r9 0 1))) 9)

;the first plus the second times a scale, as a scale then an add, in place
(defq ve_x (doubles (n2d 1) (n2d 2) (n2d 3)) ve_y (doubles (n2d 4) (n2d 5) (n2d 6)))
(assert-list-eq "nums-add-scaled doubles"
	(map (const n2i) (nums-add-scaled ve_x ve_y (n2d 2))) '(9 12 15))
(assert-true "nums-add-scaled doubles same as scale then add"
	(eql (nums-add-scaled ve_x ve_y (n2d 0.375)) (nums-add ve_x (nums-scale ve_y (n2d 0.375)))))
(nums-add-scaled ve_x ve_y (n2d -1) ve_x)
(assert-list-eq "nums-add-scaled doubles in place" (map (const n2i) ve_x) '(-3 -3 -3))

;nums-convert, every kind into every kind, each element as its scalar's
;own conversion has it: a fixed floored to an int, a double truncated
(defq ve_ns '(-2.5 -1 0 0.75 3)
	ve_kinds (list
		(apply (const nums) (map (# (n2i (n2d %0))) ve_ns))
		(apply (const fixeds) (map (const n2q) ve_ns))
		(apply (const doubles) (map (const n2d) ve_ns))
		(apply (const floats) (map (const n2f) ve_ns))))
(defun ve-shown (v) (map (# (n2q (n2d %0))) v))
(each (lambda (src)
		(each (lambda (dst)
				(defq ve_out (nums-convert src (slice dst 0 0)))
				(assert-true "nums-convert keeps its kind" (eql (type-of ve_out) (type-of dst)))
				(assert-eq "nums-convert length" 5 (length ve_out)))
			ve_kinds))
	ve_kinds)
(test-cases
	(ve-shown (nums-convert (elem-get ve_kinds 1) (nums))) '(-3.0 -1.0 0.0 0.0 3.0)
	(ve-shown (nums-convert (elem-get ve_kinds 2) (nums))) '(-2.0 -1.0 0.0 0.0 3.0)
	(ve-shown (nums-convert (elem-get ve_kinds 3) (fixeds))) '(-2.5 -1.0 0.0 0.75 3.0)
	(ve-shown (nums-convert (elem-get ve_kinds 1) (floats))) '(-2.5 -1.0 0.0 0.75 3.0)
	(ve-shown (nums-convert (elem-get ve_kinds 0) (doubles))) '(-2.0 -1.0 0.0 0.0 3.0)
	(ve-shown (nums-convert (elem-get ve_kinds 2) (floats))) '(-2.5 -1.0 0.0 0.75 3.0))
(assert-eq "nums-convert into a longer, made the source's length" 2
	(length (nums-convert (floats (n2f 1) (n2f 2)) (doubles (n2d 9) (n2d 9) (n2d 9)))))
(assert-error "nums-convert of a list" (nums-convert (list 1) (nums)))
