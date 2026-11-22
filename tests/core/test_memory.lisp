(report-header "Memory: Raw object get-xxx and set-xxx")

(defq raw_obj (str-alloc 64))

;each at its natural alignment, docs/vm/vp_vm.md
(set-byte  raw_obj 0 -1)
(set-short raw_obj 2 -1)
(set-int   raw_obj 4 -1)
(set-long  raw_obj 8 0x123456789ABCDEF0)
(set-str   raw_obj 16 "HELLO")

(assert-eq "get-ubyte" 255 (get-ubyte raw_obj 0))
(assert-eq "get-byte"  -1  (get-byte raw_obj 0))
(assert-eq "get-short" -1  (get-short raw_obj 2))
(assert-eq "get-int"   -1  (get-int raw_obj 4))

;the same of a file's or the net's bytes, the high byte first
(defq be_obj "\x12\x34\xff\xfe\x80\x00\x00\x01\x7f\xff\xff\xff")
(assert-eq "get-ushort-be" 0x1234 (get-ushort-be be_obj 0))
(assert-eq "get-ushort-be, its top bit set" 0xfffe (get-ushort-be be_obj 2))
(assert-eq "get-short-be" 0x1234 (get-short-be be_obj 0))
(assert-eq "get-short-be, less than 0" -2 (get-short-be be_obj 2))
(assert-eq "get-uint-be" 0x1234fffe (get-uint-be be_obj 0))
(assert-eq "get-uint-be, its top bit set" 0x80000001 (get-uint-be be_obj 4))
(assert-eq "get-int-be" 0x7fffffff (get-int-be be_obj 8))
(assert-eq "get-int-be, less than 0" (- 1 0x80000000) (get-int-be be_obj 4))
(assert-eq "get-long-be" 0x1234fffe80000001 (get-long-be be_obj 0))
(assert-eq "get-long-be, less than 0" (neg 0x7fffffffffffffff) (get-long-be "\x80\x00\x00\x00\x00\x00\x00\x01" 0))
;at any place, not only an aligned one
(assert-eq "get-short-be at an odd place" (- 0x34ff 0) (get-short-be be_obj 1))
(assert-eq "get-int-be at an odd place" (- 0xfffe8000 0x100000000) (get-int-be be_obj 2))
(assert-eq "get-uint-be at an odd place" 0x34fffe80 (get-uint-be be_obj 1))
(assert-eq "get-long"  0x123456789ABCDEF0 (get-long raw_obj 8))
(assert-eq "get-str"   "HELLO" (get-str raw_obj 16 5))

; --- type-to-size ---
(assert-eq "type-to-size int" +int_size (type-to-size 'i))
(assert-eq "type-to-size ubyte" +byte_size (type-to-size 'ub))
; type-to-size in root.inc has a default that returns nil or +long_size depending on match
(assert-eq "type-to-size default" :nil (type-to-size 'unknown))
