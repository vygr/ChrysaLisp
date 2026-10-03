(report-header "Stream Edges: empty streams, line ends, seeking, the reader")

(defun se-read (text &rest fns)
	; read from a stream of text with each function in turn
	(defq s (string-stream text))
	(map (# (%0 s)) fns))

; --- reading from an empty stream gives :nil ---
(test-cases
	(read-char (string-stream "")) :nil
	(read-line (string-stream "")) :nil
	(read-blk (string-stream "") 4) :nil
	(stream-avail (string-stream "")) 0
	(read-char (memory-stream)) :nil)

; --- read-line, a last line needs no line end, an empty line is "" ---
(test-cases
	(read-line (string-stream "abc")) "abc"
	(se-read "a\nb\n" read-line read-line read-line) '("a" "b" :nil)
	(se-read "a\n\nb" read-line read-line read-line read-line) '("a" "" "b" :nil)
	(se-read "\n" read-line read-line) '("" :nil))

; --- read-char and read-blk run out cleanly ---
(defq se_s (string-stream "abcdef"))
(test-cases
	(se-read "ab" read-char read-char read-char) '(97 98 :nil)
	(read-blk se_s 2) "ab"
	;a block longer than what is left gives what is left
	(read-blk se_s 10) "cdef"
	(read-blk se_s 1) :nil
	(read-blk (string-stream "abc") 0) "")

; --- stream-avail and stream-seek, whence 0 start, 1 current, 2 end ---
(defun se-seek (offset whence)
	(defq s (string-stream "abcdef"))
	(stream-seek s offset whence)
	(read-blk s 10))

(test-cases
	(stream-avail (string-stream "abc")) 3
	(se-seek 0 0) "abcdef"
	(se-seek 3 0) "def"
	(se-seek 6 0) :nil
	(se-seek -2 2) "ef")

(defq se_s (string-stream "abcdef"))
(read-char se_s)
(assert-eq "avail after a read" 5 (stream-avail se_s))
(stream-seek se_s 1 1)
(assert-eq "seek from current" "cdef" (read-blk se_s 10))

; --- writing ---
(defun se-write (fn &rest args)
	(defq s (string-stream (cat "")))
	(apply fn (cat (list s) args))
	(str s))

(test-cases
	(se-write write-char 65) "A"
	(se-write write-char (list 65 66)) "AB"
	;a width writes that many bytes, low byte first
	(se-write write-char 0x4241 2) "AB"
	(se-write write-blk "xyz") "xyz"
	(se-write write-blk "") ""
	(length (se-write write-line "")) 1
	;str of a stream gives what was written, not what there is to read
	(str (string-stream "abc")) "")

; --- binary values round trip, signed and unsigned ---
(defq se_m (memory-stream))
(write-long se_m -1) (write-int se_m -1) (write-short se_m -1) (write-byte se_m -1)
(write-int se_m -1) (write-short se_m -1) (write-byte se_m -1)
(stream-seek se_m 0 0)
(test-cases
	(read-long se_m) -1
	(read-int se_m) -1
	(read-short se_m) -1
	(read-byte se_m) -1
	(read-uint se_m) 4294967295
	(read-ushort se_m) 65535
	(read-ubyte se_m) 255
	(read-byte se_m) :nil)

;a value that runs past the end is :nil
(defq se_m (memory-stream))
(write-byte se_m 1)
(stream-seek se_m 0 0)
(assert-eq "read-long past the end" :nil (read-long se_m))

(defq se_m (memory-stream))
(write-line se_m "a") (write-line se_m "b")
(stream-seek se_m 0 0)
(assert-list-eq "memory stream lines" '("a" "b" :nil)
	(list (read-line se_m) (read-line se_m) (read-line se_m)))

; --- lines!, stops when the function gives non :nil ---
(defun se-lines (text &optional stop)
	(defq out (list))
	(lines! (lambda (line) (push out line) (eql line stop)) (string-stream text))
	out)

(test-cases
	(se-lines "") '()
	(se-lines "a\n\nb") '("a" "" "b")
	(se-lines "a\n") '("a")
	(se-lines "a\nb\nc" "b") '("a" "b"))

(defq se_idx (list))
(lines! (lambda (line) (push se_idx (!)) :nil) (string-stream "a\nb\nc"))
(assert-list-eq "lines! index" '(0 1 2) se_idx)

; --- the reader, nothing to read gives :nil ---
(defq se_r (string-stream "1 2 3"))
(test-cases
	(first (read (string-stream ""))) :nil
	(first (read (string-stream "   "))) :nil
	(first (read (string-stream "; comment"))) :nil
	(first (read (string-stream "42"))) 42
	(first (read (string-stream "(a (b) 1.5 :k)"))) '(a (b) 1.5 :k)
	(first (read se_r)) 1
	(first (read se_r)) 2
	(first (read se_r)) 3
	(first (read se_r)) :nil)

; --- files that are not there ---
(test-cases
	(file-stream "tests/no_such_file") :nil
	(load-stream "tests/no_such_file") :nil
	(load "tests/no_such_file") :nil
	(age "tests/no_such_file") 0)

; --- hex and utf8 ---
(test-cases
	(hex-encode "") ""
	(hex-encode "AB") "4142"
	(hex-decode "") ""
	(hex-decode "4142") "AB"
	(hex-decode (hex-encode "round trip")) "round trip"
	(byte-to-hex-str 0) "00"			(byte-to-hex-str 255) "FF"
	(short-to-hex-str 255) "00FF"
	(int-to-hex-str 0) "00000000"		(int-to-hex-str 255) "000000FF"
	(int-to-hex-str -1) "FFFFFFFF"
	(long-to-hex-str -1) "FFFFFFFFFFFFFFFF"
	(num-to-utf8 65) "A"
	(length (num-to-utf8 0x20ac)) 3
	(length (num-to-utf8 0x1f600)) 4)
