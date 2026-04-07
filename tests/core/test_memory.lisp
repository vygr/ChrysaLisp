(report-header "Memory: Raw object get-xxx and set-xxx")

(defq raw_obj (str-alloc 64))

(set-byte  raw_obj 0 -1)
(set-short raw_obj 1 -1)
(set-int   raw_obj 3 -1)
(set-long  raw_obj 7 0x123456789ABCDEF0)
(set-str   raw_obj 15 "HELLO")

(assert-eq "get-ubyte" 255 (get-ubyte raw_obj 0))
(assert-eq "get-byte"  -1  (get-byte raw_obj 0))
(assert-eq "get-short" -1  (get-short raw_obj 1))
(assert-eq "get-int"   -1  (get-int raw_obj 3))
(assert-eq "get-long"  0x123456789ABCDEF0 (get-long raw_obj 7))
(assert-eq "get-str"   "HELLO" (get-str raw_obj 15 5))

; --- type-to-size ---
(assert-eq "type-to-size int" +int_size (type-to-size 'i))
(assert-eq "type-to-size ubyte" +byte_size (type-to-size 'ub))
; type-to-size in root.inc has a default that returns nil or +long_size depending on match
(assert-eq "type-to-size default" :nil (type-to-size 'unknown))
