(report-header "String Utilities")

; Casing
(assert-eq "to-upper" "HELLO" (to-upper "Hello"))
(assert-eq "to-lower" "world" (to-lower "WORLD"))

; Trimming
(assert-eq "trim-start" "abc  " (trim-start "  abc  "))
(assert-eq "trim-end"   "  abc" (trim-end "  abc  "))
(assert-eq "trim"	   "abc"   (trim "  abc  "))
(assert-eq "trim custom" "abc"  (trim ",,abc.." (const (char-class ",."))))

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
(assert-eq "ascii-char" "A" (ascii-char 65))
(assert-eq "ascii-upper" 65 (ascii-upper 97)) ; 'a' -> 'A'
(assert-eq "ascii-lower" 97 (ascii-lower 65)) ; 'A' -> 'a'

; --- Time Utils ---
(assert-eq "time-in-seconds" "1.234567" (time-in-seconds 1234567))
