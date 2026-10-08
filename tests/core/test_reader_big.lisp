(report-header "Reader: a number with a point that is too big for a fixed")

;a fixed has 47 bits before its point. A number with a point that needs
;more can not be right, but it must be a number, and reading it must not
;stop the node, which on x86_64 it did
(defun rb-read (text)
	(first (read (string-stream text))))

(each (# (assert-true (cat "read " %0 ", a number comes back") (num? (rb-read %0))))
	'("1791408183000000.0" "99999999999999999.5" "140737488355328.0" "9223372036854775807.0"
	"-1791408183000000.0" "18446744073709551615.25"))
(each (# (assert-true (cat "str-to-num " %0 ", a number comes back") (num? (str-to-num %0))))
	'("1791408183000000.0" "-99999999999999999.5" "18446744073709551615.25"))
;what does fit is as it was
(assert-true "a big one that does fit" (starts-with "1000000000000.5" (str (rb-read "1000000000000.5"))))
(assert-eq "an ordinary fixed" 1.5 (rb-read "1.5"))
(assert-eq "an ordinary fixed, below 0" -2.25 (rb-read "-2.25"))
(assert-true "a small one" (starts-with "0.125" (str (rb-read "0.125"))))
(assert-eq "a whole number is not a fixed" 1791408183000000 (rb-read "1791408183000000"))
