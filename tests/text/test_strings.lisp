(report-header "Strings: types, ops, slice, splice, hex")

(import "lib/text/charclass.inc")

(defq s "Hello World")

; Type Predicates
(assert-true "str?"  (str? s))
(assert-true "num?"  (num? 123))
(assert-true "list?" (list? (list 1 2)))
(assert-true "sym?"  (sym? 'abc))
(assert-eq   "Type-of" :num (last (type-of 42)))

(assert-eq "sym" 'hello (sym "hello"))
(assert-true "gensym" (sym? (gensym)))

; String Ops
(assert-eq "String Len" 11 (length s))
(assert-eq "Char access" (ascii-code "e") (code (elem-get s 1)))
(assert-eq "Concatenation" "AB" (cat "A" "B"))
(assert-eq "Slice" "Hello" (slice s 0 5))

; Splitting
(defq parts (split "a,b,c" ","))
(assert-eq "Split len" 3 (length parts))
(assert-eq "Split val" "b" (second parts))

; Search
(assert-eq "Find char" 4 (find "o" s))


(defq s_adv "0123456789")

; --- Slicing (Forward) ---
(assert-eq "Slice whole"  "0123456789" (slice s_adv 0 10))
(assert-eq "Slice whole2" "0123456789" (slice s_adv 0 -1))
(assert-eq "Slice mid"	"234"		(slice s_adv 2 5))
(assert-eq "Slice start"  "01"		 (slice s_adv 0 2))
(assert-eq "Slice end"	"89"		 (slice s_adv 8 10))

; --- Slicing (Reverse) ---
; If start > end, slice returns reversed substring
(assert-eq "Reverse sub"  "432"		(slice s_adv 5 2))
(assert-eq "Reverse 210"  "210"		(slice s_adv 3 0))

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

(assert-eq "expand" "    b" (expand "\tb" 4))
(assert-eq "compress" "\tb" (compress "    b" 4))
(assert-eq "cmp" 0 (cmp "abc" "abc"))
(assert-true "cmp <" (< (cmp "abc" "def") 0))
(assert-true "cmp >" (> (cmp "def" "abc") 0))

(assert-eq "hex-encode" "414243" (hex-encode "ABC"))
(assert-eq "hex-decode" "ABC" (hex-decode "414243"))

