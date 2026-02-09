;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_str_adv.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "String Advanced: Slice, Reverse, Splice")

(defq s "0123456789")

; --- Slicing (Forward) ---
(assert-eq "Slice whole"  "0123456789" (slice s 0 10))
(assert-eq "Slice whole2" "0123456789" (slice s 0 -1))
(assert-eq "Slice mid"    "234"        (slice s 2 5))
(assert-eq "Slice start"  "01"         (slice s 0 2))
(assert-eq "Slice end"    "89"         (slice s 8 10))

; --- Slicing (Reverse) ---
; If start > end, slice returns reversed substring
(assert-eq "Reverse sub"  "432"        (slice s 5 2))
(assert-eq "Reverse 210"  "210"        (slice s 3 0))

; --- Reversing ---
; The reverse macro uses (slice s -1 0)
(defq r "ABC")
(assert-eq "Reverse macro" "CBA" (reverse r))

; --- Splicing / Mutation Macros ---
(defq txt "Hello World")

; Insert: (insert seq pos part)
; Splices 'part' into 'seq' at 'pos'
; CORRECTED ARG ORDER: (insert txt 6 "Cruel ")
(assert-eq "Insert" "Hello Cruel World" (insert txt 6 "Cruel "))

; Erase: (erase seq start end)
; Removes range [start, end)
; Remove " World" (index 5 to 11)
(assert-eq "Erase" "Hello" (erase txt 5 11))

; Replace: (replace seq start end new_part)
; Replaces range [start, end) with new_part
(assert-eq "Replace" "Hello Lisp" (replace txt 6 11 "Lisp"))

; --- Low-level Splice ---
; (splice src1 src2 nums_indices)
; Toggles between src1 and src2 for each pair of indices in nums.
(defq s1 "A" s2 "B")
; Logic: 
; 1. Src1 [0,1] -> "A"
; 2. Src2 [0,1] -> "B"
; 3. Src1 [0,1] -> "A"
(defq idxs (nums 0 1 0 1 0 1))
(assert-eq "Splice LowLevel" "ABA" (splice s1 s2 idxs))

; Complex Splice (Reordering)
; Use one string source to reorder.
; s3="123"
; [2,3) -> "3" (src1)
; [1,2) -> "2" (src2)
; [0,1) -> "1" (src1)
(defq s3 "123")
(defq idxs_reorder (nums 2 3 1 2 0 1)) 
(assert-eq "Splice Reorder" "321" (splice s3 s3 idxs_reorder))
