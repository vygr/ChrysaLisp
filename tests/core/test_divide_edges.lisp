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
