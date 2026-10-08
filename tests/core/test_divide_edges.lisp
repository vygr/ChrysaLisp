(report-header "Numbers: a divide whose answer does not fit")

;the most negative number divided by -1 has an answer one too big to be a
;number. Every CPU must give the same, the number back and nothing left
;over, and none may stop for it, which x86_64 did
(defq de_min -9223372036854775808)
(assert-eq "the most negative number by -1" de_min (/ de_min -1))
(assert-eq "and nothing left over" 0 (% de_min -1))
(assert-list-eq "a nums of them" (list de_min -9 -4) (nums-div (nums de_min 9 -9) (nums -1 -1 2)))
(assert-list-eq "and what is left over" '(0 0 -1) (nums-mod (nums de_min 9 -9) (nums -1 -1 2)))
;a divide by -1 that does fit is as it ever was, and so are the rest
(test-cases
	(/ 7 -1) -7
	(% 7 -1) 0
	(/ -7 -1) 7
	(/ -7 2) -3
	(% -7 2) -1
	(/ 7 2) 3
	(% 7 -2) 1
	(/ 9223372036854775807 -1) -9223372036854775807
	(/ 100 -1 -1) 100
	(/ 3.0 -1.0) -3.0
	(/ 1.0 4.0) 0.25
	(% 7.5 2.0) 1.5
	(/ -3.0 -1.0) 3.0)
;a divide by nothing is an error where errors are checked, not an answer
(assert-error "a number by 0" (/ 1 0))
(assert-error "what is left of a number by 0" (% 1 0))
(assert-error "a fixed by 0" (/ 1.5 0.0))
(assert-error "a nums by a nums with a 0 in it" (nums-div (nums 1 2) (nums 1 0)))

;a divide in native code, with no check in front of it, means the one thing
;on every CPU. By 0 the answer is 0, and what is left over is the number.
;A typed function is such a divide, the language of the shaders checks
;nothing, so it is what shows what the CPU itself is made to do
(import "lib/gpu/vp.inc")
(defq de_prog (shader-compile (shader-read (string-stream (cat
		"(defun quot :int ((a :int) (b :int)) (/ a b))"
		"(defun both :int ((a :int) (b :int)) (+ (* (/ a b) 1000) (- a (* (/ a b) b))))"))))
	de_quot (shader-vp-func de_prog 'quot) de_both (shader-vp-func de_prog 'both))
(assert-eq "native, 7 by 0 is 0" 0 (de_quot 7 0))
(assert-eq "native, -7 by 0 is 0" 0 (de_quot -7 0))
(assert-eq "native, 0 by 0 is 0" 0 (de_quot 0 0))
(assert-eq "native, the most negative number by 0 is 0" 0 (de_quot de_min 0))
(assert-eq "native, the most negative number by -1 is itself" de_min (de_quot de_min -1))
(assert-eq "native, and the node carries on, 7 by 2" 3 (de_quot 7 2))
(assert-eq "native, -7 by 2" -3 (de_quot -7 2))
(assert-eq "native, 7 by -1" -7 (de_quot 7 -1))
(assert-eq "native, an answer and what is left, 17 by 5" 3002 (de_both 17 5))
(assert-eq "native, an answer and what is left, 17 by 0" 17 (de_both 17 0))

;it is a number of 64 bits that is divided, on every CPU. Big numbers of
;either sign, where a divide of 128 bits with the wrong top half would go
;wrong, or stop
(assert-eq "native, a big number by a small one" 4611686018427387903 (de_quot 9223372036854775807 2))
(assert-eq "native, a big number below 0 by a small one" -4611686018427387904 (de_quot de_min 2))
(assert-eq "native, a big number by 1" 9223372036854775807 (de_quot 9223372036854775807 1))
(assert-eq "native, the most negative number by 1" de_min (de_quot de_min 1))
(assert-eq "native, a small number by a big one" 0 (de_quot 5 9223372036854775807))
(assert-eq "native, what is left of a big one" 1 (- (de_both 9223372036854775807 2) (* 4611686018427387903 1000)))
(test-cases
	(/ 9223372036854775807 3) 3074457345618258602
	(% 9223372036854775807 3) 1
	(/ -9223372036854775807 3) -3074457345618258602
	(partition (list 1 2 3 4 5) 2) '((1 2) (3 4) (5))
	(str-to-num "123.5") 123.5)
