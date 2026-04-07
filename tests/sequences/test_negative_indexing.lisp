(report-header "Negative Indexing: elem-get, slice, splice")

; --- Strings ---
(defq s "012345")
(assert-eq "elem-get string -2" "5" (elem-get s -2))
(assert-eq "elem-get string -3" "4" (elem-get s -3))
(assert-eq "slice string 0 -1"  "012345" (slice s 0 -1))
(assert-eq "slice string 0 -2"  "01234"  (slice s 0 -2))
(assert-eq "slice string -3 -1" "45"	 (slice s -3 -1))

; splice with negative indices
; s[0, -2] -> "01234"
; s[-2, -1] -> "5"
(assert-eq "splice string neg" "012345" (splice s s (nums 0 -2 -2 -1)))

; --- Arrays ---
(defq a (array 10 20 30 40))
(assert-eq "elem-get array -2" 40 (elem-get a -2))
(assert-true "slice array 0 -1" (equal? (array 10 20 30 40) (slice a 0 -1)))
(assert-true "slice array -3 -1" (equal? (array 30 40) (slice a -3 -1)))

; --- Lists ---
(defq l (list 'a 'b 'c))
(assert-eq "elem-get list -2" 'c (elem-get l -2))
(assert-list-eq "slice list 0 -1" (list 'a 'b 'c) (slice l 0 -1))
(assert-list-eq "slice list -2 -1" (list 'c) (slice l -2 -1))

; --- Combined Dual Splice with Negative Indices ---
(defq s1 "ABC" s2 "123")
; s1[0, -1] -> "ABC"
; s2[-4, -1] -> "123"  (-4 is length+1-4 = 3+1-4 = 0)
(assert-eq "splice dual neg" "ABC123" (splice s1 s2 (nums 0 -1 -4 -1)))

; --- Reverse with negative ---
; s = "012345"
; slice -1 -7 -> "543210" (-1 is 6, -7 is 0)
(assert-eq "slice neg reverse" "543210" (slice s -1 -7))
