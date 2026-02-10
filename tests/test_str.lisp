;;;;;;;;;;;;;;;;;;;;;
; tests/test_str.lisp
;;;;;;;;;;;;;;;;;;;;;
(report-header "Strings & Types")

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