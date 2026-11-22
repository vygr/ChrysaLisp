(report-header "String Utilities")

; Casing
(assert-eq "to-upper" "HELLO" (to-upper "Hello"))
(assert-eq "to-lower" "world" (to-lower "WORLD"))

; Trimming
(assert-eq "trim-start" "abc  " (trim-start "  abc  "))
(assert-eq "trim-end"   "  abc" (trim-end "  abc  "))
(assert-eq "trim"	   "abc"   (trim "  abc  "))

; Padding
(assert-eq "pad left"  "0042" (pad "42" 4 "0"))
(assert-eq "pad right" "abc  " (cat "abc" (pad "" 2 " "))) ; pad is primarily left pad in root.inc

; Predicates
(assert-true "starts-with" (starts-with "He" "Hello"))
(assert-true "ends-with"   (ends-with "lo" "Hello"))

; Conversions
(assert-eq "str-to-num" 123 (str-to-num "123"))
(assert-eq "char conversion" "A" (char 65))
(assert-eq "code conversion" 65 (code "A"))

; --- Reflow ---
(defq words (list "The" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog"))
(defq lines (reflow words 15))
(assert-list-eq "reflow" '("The quick" "brown fox" "jumps over the" "lazy dog") lines)

; --- Math Utils ---
(assert-eq "align 7 4" 8 (align 7 4))
(assert-eq "align 8 4" 8 (align 8 4))

; --- Hex / UTF8 ---
(assert-eq "num-to-utf8" "A" (num-to-utf8 65))
(assert-eq "byte-to-hex-str" "FF" (byte-to-hex-str 255))
(assert-eq "short-to-hex-str" "FFFF" (short-to-hex-str 0xFFFF))
(assert-eq "int-to-hex-str" "12345678" (int-to-hex-str 0x12345678))
(assert-eq "long-to-hex-str" "123456789ABCDEF0" (long-to-hex-str 0x123456789ABCDEF0))

; --- ASCII Utils ---
(assert-eq "ascii-code" 65 (ascii-code "A"))
(assert-eq "ascii-upper" 65 (ascii-upper 97)) ; 'a' -> 'A'
(assert-eq "ascii-lower" 97 (ascii-lower 65)) ; 'A' -> 'a'

; --- Time Utils ---
(assert-eq "time-in-seconds" "1.234567" (time-in-seconds 1234567))
(assert-true "time-add gives what its body gives"
	(starts-with "42\n" (test-output "(print (time-add {su_x} 42)) (time-report)")))
(defq su_report (test-output "(time-add {su_y} 1) (time-add {su_y} 2) (time-report)"))
(assert-true "time-report says the heading, its total, how many times and the longest"
	(and (starts-with "su_y 0." su_report) (found? su_report " 2x longest 0.")))

; --- String root parsing utilities ---
(assert-eq "str-as-num" 123 (str-as-num "123"))

(assert-eq "str-to-double int" (n2d 123) (str-to-double "123"))
(assert-eq "str-to-double zero" (n2d 0) (str-to-double "0"))
(assert-eq "str-to-double neg int" (n2d -45) (str-to-double "-45"))
(assert-true "str-to-double returns double" (double? (str-to-double "1")))

; Float tests
(defq f05 (str-to-double "0.5"))
(assert-true "str-to-double 0.5 is double" (double? f05))
(assert-eq "str-to-double 0.5 type" :double (last (type-of f05)))

; Scientific notation tests
(assert-eq "str-to-double 1e2" (n2d 100) (str-to-double "1e2"))
(assert-true "str-to-double 1e-2" (double? (str-to-double "1e-02")))
(assert-true "str-to-double 9.97231e-09" (double? (str-to-double "9.97231e-09")))
(assert-eq "str-to-double upper E" (n2d 150) (str-to-double "1.5E2"))
(assert-eq "str-to-double exp plus" (n2d 12300) (str-to-double "1.23e+04"))
(assert-eq "str-to-double mantissa plus" (str-to-double "42.5") (str-to-double "+42.5"))
(assert-eq "str-to-double neg sci" (str-to-double "-0.0015") (str-to-double "-1.5e-3"))

; --- double-to-str Tests ---
(assert-eq "double-to-str zero" "0.0" (double-to-str (n2d 0)))
(assert-eq "double-to-str int" "42.0" (double-to-str (n2d 42)))
(assert-eq "double-to-str neg int" "-45.0" (double-to-str (n2d -45)))
(assert-eq "double-to-str simple dec" "0.5" (double-to-str (str-to-double "0.5")))
(assert-eq "double-to-str dec" "123.456" (double-to-str (str-to-double "123.456")))
(assert-eq "double-to-str neg dec" "-123.456" (double-to-str (str-to-double "-123.456")))
(assert-eq "double-to-str small sci" "9.97231e-09" (double-to-str (str-to-double "9.97231e-09")))
(assert-eq "double-to-str large sci" "1.5e+14" (double-to-str (str-to-double "1.5e+14")))
(assert-eq "double-to-str neg sci" "-1.5e-06" (double-to-str (str-to-double "-1.5e-06")))
(assert-eq "double-to-str precision" "1.2346" (double-to-str (str-to-double "1.23456789") 4))
(assert-eq "double-to-str non-double arg" "100.0" (double-to-str 100))

; --- Bidirectional Roundtrip Tests ---
(assert-eq "roundtrip decimal" (str-to-double "123.456") (str-to-double (double-to-str (str-to-double "123.456"))))
(assert-eq "roundtrip small sci" (str-to-double "9.97231e-09") (str-to-double (double-to-str (str-to-double "9.97231e-09"))))
(assert-eq "roundtrip large sci" (str-to-double "1.5e+14") (str-to-double (double-to-str (str-to-double "1.5e+14"))))
(assert-eq "roundtrip neg" (str-to-double "-42.5") (str-to-double (double-to-str (str-to-double "-42.5"))))

(defq cs "ABC\x00DEF")
(assert-eq "get-cstr" "ABC" (get-cstr cs 0))

;a time as a player shows it
(assert-list-eq "time-as-clock, m:ss and h:mm:ss" '("0:00" "1:05" "61:01" "1:01:01" "0:00:09")
	(list (time-as-clock 0) (time-as-clock 65) (time-as-clock 3661) (time-as-clock 3661 :t) (time-as-clock 9 :t)))
