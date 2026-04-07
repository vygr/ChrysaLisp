(report-header "Sequence Splice: Mixed, Reversed, Dual Input")

; Note: ChrysaLisp slice/splice reversed indices:
; If start > end, it takes indices (start-1) down to end.
; e.g. (slice "0123" 2 0) -> "10" (indices 1, 0)

; --- Strings ---
(defq s1 "ABC" s2 "123")
; 1. Dual input, mixed directions
; s1[0,1] -> "A" (forward)
; s2[2,1] -> "2" (reverse: index 1)
; s1[2,3] -> "C" (forward)
; s2[3,0] -> "321" (reverse: indices 2, 1, 0)
(defq idxs1 (nums 0 1 2 1 2 3 3 0))
(assert-eq "Splice String: Dual Mixed" "A2C321" (splice s1 s2 idxs1))

; --- Arrays ---
(defq a1 (array 10 20 30) a2 (array 1 2 3))
; a1[0,2] -> (10 20)
; a2[2,0] -> (2 1)
; a1[2,3] -> (30)
(defq idxs2 (nums 0 2 2 0 2 3))
(assert-true "Splice Array: Dual Mixed" (equal? (array 10 20 2 1 30) (splice a1 a2 idxs2)))

; --- Lists ---
(defq l1 (list 'a 'b 'c) l2 (list 'x 'y 'z))
; l1[0,1] -> (a)
; l2[2,1] -> (y)
; l1[2,3] -> (c)
(defq idxs3 (nums 0 1 2 1 2 3))
(assert-list-eq "Splice List: Dual Mixed" (list 'a 'y 'c) (splice l1 l2 idxs3))

; --- Mixed: Forward and Reverse same source ---
(defq s3 "012345")
; s3[0,2] -> "01"
; s3[6,4] -> "54" (indices 5, 4)
; s3[2,3] -> "2" (index 2)
(defq idxs4 (nums 0 2 6 4 2 3))
(assert-eq "Splice String: Same Mixed" "01542" (splice s3 s3 idxs4))

; --- Nums ---
(defq n1 (nums 100 200 300) n2 (nums 1 2 3))
; n1[0,1] -> 100
; n2[3,0] -> 3 2 1
(defq idxs5 (nums 0 1 3 0))
(assert-true "Splice Nums: Dual Mixed" (equal? (nums 100 3 2 1) (splice n1 n2 idxs5)))

; --- Fixeds ---
(defq f1 (fixeds 1.5 2.5 3.5) f2 (fixeds 0.1 0.2 0.3))
; f1[0,1] -> 1.5
; f2[2,1] -> 0.2
(defq idxs6 (nums 0 1 2 1))
(assert-true "Splice Fixeds: Dual Mixed" (equal? (fixeds 1.5 0.2) (splice f1 f2 idxs6)))
