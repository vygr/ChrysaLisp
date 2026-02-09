;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests/test_low_level_io.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(report-header "Low-Level IO: get/set and read/write")

; --- get-xxx and set-xxx (Raw Object access) ---
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

; --- read-xxx and write-xxx (Stream access) ---
(defq ms (memory-stream))

(write-byte  ms 0xAA)
(write-short ms 0xBBCC)
(write-int   ms 0xDDEEFF00)
(write-long  ms 0x1122334455667788)

(stream-seek ms 0 0)

(assert-eq "read-ubyte" 0xAA (read-ubyte ms))
(assert-eq "read-ushort" 0xBBCC (read-ushort ms))
(assert-eq "read-uint" 0xDDEEFF00 (read-uint ms))
(assert-eq "read-long" 0x1122334455667788 (read-long ms))

; Test signed reads
(stream-seek ms 0 0)
(write-byte ms -1)
(write-short ms -1)
(stream-seek ms 0 0)
(assert-eq "read-byte signed"  -1 (read-byte ms))
(assert-eq "read-short signed" -1 (read-short ms))
