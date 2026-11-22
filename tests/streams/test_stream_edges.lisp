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
	(stream-avail (string-stream "")) 0)

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
	(se-seek 6 0) :nil)

(defq se_s (string-stream "abcdef"))
(read-char se_s)
(assert-eq "avail after a read" 5 (stream-avail se_s))

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

;what is neither a num nor a list of them is an error that is told, not a
;crash: the check of the arguments had lost them (2026-11-21)
(defun se-bad (arg)
	(catch (progn (write-char (string-stream (cat "")) arg 1) :wrote) :error))
(test-cases
	(se-bad "ab") :error
	(se-bad (nums 65 66)) :error
	(se-bad (list)) :error
	(se-bad (list 65 "b")) :error
	(se-bad (list 65 66)) :wrote)

; --- binary values round trip, signed and unsigned ---
(defq se_m (memory-stream))
(write-long se_m -1) (write-int se_m -1)
(stream-seek se_m 0 0)
(test-cases
	(read-long se_m) -1
	(read-int se_m) -1)

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

;bits written through a pool and flushed read back through another
(defq se_m (memory-stream) se_wpool (array 0 0) se_rpool (array 0 0))
(write-bits se_m se_wpool 0xA 4)
(write-bits se_m se_wpool 0x5 4)
(flush-bits se_m se_wpool)
(stream-seek se_m 0 0)
(assert-eq "read-bits 1" 0xA (read-bits se_m se_rpool 4))
(assert-eq "read-bits 2" 0x5 (read-bits se_m se_rpool 4))

; --- lines!, stops when the function gives non :nil ---
(defun se-lines (text &optional stop)
	(defq out (list))
	(lines! (lambda (line) (push out line) (eql line stop)) (string-stream text))
	out)

(test-cases
	(se-lines "") '()
	(se-lines "a\n\nb") '("a" "" "b"))

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
	(hex-decode "") ""
	(byte-to-hex-str 0) "00"
	(short-to-hex-str 255) "00FF"
	(int-to-hex-str 0) "00000000"		(int-to-hex-str 255) "000000FF"
	(int-to-hex-str -1) "FFFFFFFF"
	(long-to-hex-str -1) "FFFFFFFFFFFFFFFF"
	(length (num-to-utf8 0x20ac)) 3
	(length (num-to-utf8 0x1f600)) 4)

; --- an array of numbers written to a stream and read from one ---
(import "lib/streams/nums.inc")
(defq se_path (path 1.5 2.5 -3.25 1000000.125) se_ss (string-stream (cat "")))
(write-long se_ss 77)
(assert-eq "write-nums, the bytes of a path's elements" 32 (write-nums se_ss se_path))
(assert-eq "of an array of none, nothing" 0 (write-nums se_ss (path)))
(assert-eq "a floats is 4 bytes an element" 8 (write-nums se_ss (floats 1.5 2.5)))
(defq se_rs (string-stream (str se_ss)))
(assert-eq "a number before it" 77 (read-long se_rs))
(assert-true "read-nums, the path again" (eql se_path (read-nums se_rs 32 (path))))
(assert-eq "no bytes is the array as it was" 1 (length (read-nums se_rs 0 (path 9.0))))
(assert-eq "onto the end of what an array has, a floats" 3 (length (read-nums se_rs 8 (floats 0.5))))
(assert-eq "at the stream's end, none" :nil (read-nums se_rs 8 (path)))
