(report-header "Streams & IO: read/write and files")

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
(assert-eq "lines! single eof return" :nil (lines! (lambda (l) (++ line_count_test) :nil) ms2))
(assert-eq "lines!" 1 line_count_test)

; lines! multi-line full iteration
(defq ms_lines (memory-stream))
(write-line ms_lines "Line 0")
(write-line ms_lines "Line 1")
(write-line ms_lines "Line 2")
(write-line ms_lines "Line 3")
(stream-seek ms_lines 0 0)
(defq collected (list) indices (list))
(defq ret (lines! (# (push indices (!)) (push collected %0) :nil) ms_lines))
(assert-eq "lines! eof return" :nil ret)
(assert-eq "lines! count" 4 (length collected))
(assert-eq "lines! line 0" "Line 0" (first collected))
(assert-eq "lines! line 3" "Line 3" (elem-get collected 3))
(assert-eq "lines! idx 0" 0 (first indices))
(assert-eq "lines! idx 3" 3 (elem-get indices 3))

; lines! early breakout on non-nil return
(stream-seek ms_lines 0 0)
(defq ret (lines! (# (if (eql %0 "Line 1") "Found Line 1")) ms_lines))
(assert-eq "lines! breakout value" "Found Line 1" ret)
(assert-eq "lines! stream pos after break" "Line 2" (read-line ms_lines))

; lines! start and end bounds
(stream-seek ms_lines 0 0)
(setq collected (list))
(defq ret (lines! (# (push collected %0) :nil) ms_lines 1 3))
(assert-eq "lines! bounded return" :nil ret)
(assert-eq "lines! bounded count" 2 (length collected))
(assert-eq "lines! bounded start" "Line 1" (first collected))
(assert-eq "lines! bounded end" "Line 2" (elem-get collected 1))

; lines! error propagation
(stream-seek ms_lines 0 0)
(defq err_ret (catch (lines! (# (if (eql %0 "Line 1") (throw "test error"))) ms_lines) 'caught))
(assert-eq "lines! error propagation" 'caught err_ret)

; --- File and String Save/Load ---
(defq test_file_io "tmp_test_file_io.txt")
(save "Hello World" test_file_io)
(assert-eq "save/load" "Hello World" (load test_file_io))
(defq fs_test (file-stream test_file_io))
(assert-true "file-stream" (not (nil? fs_test)))
(assert-eq "file-stream read" "Hello" (read-blk fs_test 5))
(pii-remove test_file_io)

; --- Print and Prin Return Values ---
(assert-eq "print no args" :nil (print))
(assert-eq "print with args" :nil (print "  [print test output]"))
(assert-eq "prin no args" :nil (prin))
(assert-eq "prin with args" :nil (prin "  [prin test output]\n"))

; --- lines! with print directly ---
(stream-seek ms_lines 0 0)
(assert-eq "lines! print directly" :nil (lines! print ms_lines 0 2))
