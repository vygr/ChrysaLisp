(report-header "Streams: EOF behaviour")

(defun test-eof-on-stream (ms name)
	(assert-eq (cat name ": read-char") :nil (read-char ms))
	(assert-eq (cat name ": read-blk") :nil (read-blk ms 10))
	(assert-eq (cat name ": read-line") :nil (read-line ms))
	(assert-eq (cat name ": read-ubyte") :nil (read-ubyte ms))
	(assert-eq (cat name ": read-ushort") :nil (read-ushort ms))
	(assert-eq (cat name ": read-uint") :nil (read-uint ms))
	(assert-eq (cat name ": read-byte") :nil (read-byte ms))
	(assert-eq (cat name ": read-short") :nil (read-short ms))
	(assert-eq (cat name ": read-int") :nil (read-int ms))
	(assert-eq (cat name ": read-long") :nil (read-long ms))
	
	; read-bits returns -1 on EOF
	(defq r_bit_pool (array 0 0))
	(assert-eq (cat name ": read-bits") -1 (read-bits ms r_bit_pool 8)))

; Test on empty memory stream
(test-eof-on-stream (memory-stream) "empty-ms")

; Test on memory stream after reading all content
(defq ms (memory-stream))
(write-line ms "hello")
(stream-seek ms 0 0)
(assert-eq "ms-read-all: read-line" "hello" (read-line ms))
(test-eof-on-stream ms "ms-at-eof")

; Test read-avail on empty stream
(defq ms_empty (memory-stream))
(assert-eq "empty-ms: read-avail" "" (read-avail ms_empty))

; Test read-avail at EOF (should also be empty string based on implementation)
(defq ms_eof (memory-stream))
(write-blk ms_eof "data")
(stream-seek ms_eof 0 0)
(read-blk ms_eof 4)
(assert-eq "ms-at-eof: read-avail" "" (read-avail ms_eof))
