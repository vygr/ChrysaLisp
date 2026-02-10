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

; Additional Stream tests
(defq ms2 (memory-stream))
(write-char ms2 (ascii-code "A"))
(write-blk ms2 "BC")
(stream-seek ms2 0 0)
(assert-eq "read-char" (ascii-code "A") (read-char ms2))
(assert-eq "read-blk" "BC" (read-blk ms2 2))
(assert-eq "stream-avail" 0 (stream-avail ms2))

(stream-seek ms2 0 0)
(write-line ms2 "Hello")
(stream-seek ms2 0 0)
(assert-eq "read-line" "Hello" (read-line ms2))

; lines!
(stream-seek ms2 0 0)
(defq line_count_test 0)
(lines! (lambda (l) (++ line_count_test)) ms2)
(assert-eq "lines!" 1 line_count_test)

; --- File and String Save/Load ---
(defq test_file_io "tmp_test_file_io.txt")
(save "Hello World" test_file_io)
(assert-eq "save/load" "Hello World" (load test_file_io))
(defq fs_test (file-stream test_file_io))
(assert-true "file-stream" (not (nil? fs_test)))
(assert-eq "file-stream read" "Hello" (read-blk fs_test 5))
(pii-remove test_file_io)
