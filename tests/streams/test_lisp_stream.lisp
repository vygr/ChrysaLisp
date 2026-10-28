(report-header "Streams: a stream read from Lisp functions, lisp-stream")

(import "lib/streams/lisp_stream.inc")

;a stream of pieces, that a function gives one at a time, and of where it is
(defq ls_parts (list "hello " "wor" "ld" "\nsecond line\n" "third")
	ls_all (apply (const cat) ls_parts))

(defun ls-pieces (pieces)
	;a state that fill and seek are given: the pieces, which is next, how
	;much of it is not wanted, and how many times it was asked
	(list pieces 0 0 0))

(defun ls-fill (st)
	(bind '(pieces at drop asked) st)
	(elem-set st 3 (inc asked))
	(when (< at (length pieces))
		(elem-set st 1 (inc at))
		(elem-set st 2 0)
		(slice (elem-get pieces at) drop -1)))

(defun ls-seek (st offset whence)
	;where the pieces begin, and so where it is and where the end is
	(bind '(pieces at drop asked) st)
	(defq starts (reduce (# (push %0 (+ (last %0) (length %1)))) pieces (list 0))
		pos (max 0 (min (last starts) (cond
			((= whence 0) offset)
			((= whence 1) (+ offset (elem-get starts at) drop))
			((+ (last starts) offset)))))
		to (some (# (if (< pos (elem-get starts (inc %0))) %0)) (range 0 (length pieces))))
	(elem-set st 1 (ifn to (length pieces)))
	(elem-set st 2 (if to (- pos (elem-get starts to)) 0))
	pos)

(defun ls-open (pieces &optional no_seek)
	(defq st (ls-pieces pieces))
	(list st (if no_seek (lisp-stream `(lambda () (,ls-fill ',st)))
		(lisp-stream `(lambda () (,ls-fill ',st)) `(lambda (offset whence) (,ls-seek ',st offset whence))))))

;read
(bind '(ls_st ls_s) (ls-open ls_parts))
(assert-true "it is a stream" (find :stream (type-of ls_s)))
(assert-eq "nothing is asked for till it is read" 0 (elem-get ls_st 3))
(assert-eq "a line, across three pieces" "hello world" (read-line ls_s))
(assert-eq "the next" "second line" (read-line ls_s))
(assert-eq "the last, with no line end" "third" (read-line ls_s))
(assert-eq "and no more" :nil (read-line ls_s))
(assert-eq "nor a char" :nil (read-char ls_s))

(bind '(ls_st ls_s) (ls-open ls_parts))
(assert-eq "a char" (ascii-code "h") (read-char ls_s))
(assert-eq "a block, across pieces" "ello wo" (read-blk ls_s 7))
(assert-eq "a piece is asked for when it is wanted: two were" 2 (elem-get ls_st 3))

(bind '(ls_st ls_s) (ls-open ls_parts))
(defq ls_lines (list))
(lines! (# (push ls_lines %0) :nil) ls_s)
(assert-list-eq "(lines!) of it" '("hello world" "second line" "third") ls_lines)

;what ends it
(assert-eq "no pieces, no chars" :nil (read-char (second (ls-open (list)))))
(defq ls_s (lisp-stream (lambda () "")))
(assert-eq "an empty str is the end" :nil (read-char ls_s))
(defq ls_s (lisp-stream (lambda () 42)))
(assert-eq "what is not a str is the end" :nil (read-char ls_s))
(defq ls_s (lisp-stream (lambda () (throw "no" 1))))
(assert-eq "a function that throws is the end" :nil (read-char ls_s))

;seek
(bind '(ls_st ls_s) (ls-open ls_parts))
(assert-eq "sought from the start" 6 (stream-seek ls_s 6 0))
(assert-eq "read from there" "world" (read-blk ls_s 5))
(assert-eq "where it is, with nothing sought" 11 (stream-seek ls_s 0 1))
(assert-eq "read from there still" 10 (read-char ls_s))
(assert-eq "sought back from where it is" 2 (stream-seek ls_s -10 1))
(assert-eq "read from there" "llo" (read-blk ls_s 3))
(assert-eq "sought on from where it is, what was had and not read is not counted" 7 (stream-seek ls_s 2 1))
(assert-eq "read from there" "orld" (read-blk ls_s 4))
(assert-eq "sought from the end" (- (length ls_all) 5) (stream-seek ls_s -5 2))
(assert-eq "read from there" "third" (read-blk ls_s 5))
(assert-eq "sought past the end is the end" (length ls_all) (stream-seek ls_s 1000 0))
(assert-eq "and nothing is read" :nil (read-char ls_s))
(assert-eq "sought before the start is the start" 0 (stream-seek ls_s -1000 0))
(assert-eq "and it is read again from there" "hello" (read-blk ls_s 5))

(assert-eq "no seek function: it can not be sought" -1 (stream-seek (second (ls-open ls_parts :t)) 0 0))
(defq ls_s (lisp-stream (lambda () "abc") (lambda (offset whence) :nil)))
(assert-eq "a seek function that gives no number: it was not" -1 (stream-seek ls_s 0 0))
(defq ls_s (lisp-stream (lambda () "abc") (lambda (offset whence) (throw "no" 1))))
(assert-eq "nor one that throws" -1 (stream-seek ls_s 0 0))

;many, made and let go of, and long ones
(defq ls_sum 0)
(times 2000 (setq ls_sum (+ ls_sum (read-char (second (ls-open (list "a" "b")))))))
(assert-eq "two thousand made, read once and let go of" (* 2000 (ascii-code "a")) ls_sum)
(defq ls_long (map (# (pad "" 1000 (char (+ 65 (% %0 26))))) (range 0 300)) ls_count 0)
(bind '(ls_st ls_s) (ls-open ls_long))
(while (defq ls_block (read-blk ls_s 7777)) (setq ls_count (+ ls_count (length ls_block))))
(assert-eq "300,000 bytes in 300 pieces, read in blocks of another length" 300000 ls_count)

;the examples of docs/ai_digest/streams.md, "A Stream Of Your Own"
(import "lib/hash/sums.inc")
(import "lib/streams/deflate.inc")

;the parts of a list, one after the other
(defun parts-fill (parts)
	;the next of them, :nil after the last
	(if (nempty? parts) (pop parts)))
(defq parts (reverse (list "one\n" "two\n" "three\n"))
	in (lisp-stream `(lambda () (,parts-fill ',parts))) got (list))
(lines! (# (push got %0) :nil) in)
(assert-list-eq "parts" '("one" "two" "three") got)

;another stream, changed as it is read
(defun upper-fill (in)
	(if (defq part (read-blk in 4096)) (to-upper part)))
(defq up (lisp-stream `(lambda () (,upper-fill ',(string-stream "quietly now")))))
(assert-eq "upper" "QUIETLY NOW" (read-blk up 100))

;what is written summed on its way to another stream
(defun summed-write (state data flush)
	(bind '(out &ignore) state)
	(elem-set state 1 (crc32 data (second state)))
	(write-blk out data)
	(if flush (stream-flush out)))
(defq to (memory-stream) state (list to 0)
	summed (lisp-stream '(lambda () :nil) :nil `(lambda (data flush) (,summed-write ',state data flush))))
(write-blk summed "The quick brown fox jumps over the lazy dog")
(stream-flush summed)
(assert-eq "summed" 0x414fa339 (second state))
(stream-seek to 0 0)
(assert-eq "and passed on" "The quick brown fox jumps over the lazy dog" (read-blk to 100))

;and anything that reads a stream reads one, native code too: inflate of a
;zlib stream given a byte at a time
(defq z (memory-stream))
(deflate-compress (string-stream "hello hello hello hello") z :t)
(stream-seek z 0 0)
(defq zdata (read-blk z 1000) zat 0 back (memory-stream))
(defun byte-fill (data)
	(when (< zat (length data))
		(setq zat (inc zat))
		(slice data (dec zat) zat)))
(assert-eq "inflate a byte at a time" :t (inflate-decompress (lisp-stream `(lambda () (,byte-fill ',zdata))) back :t))
(stream-seek back 0 0)
(assert-eq "what it was" "hello hello hello hello" (read-blk back 100))

;a fill that gives what is not a str, nor :nil, ends the stream of that
;fault: what is read before it is read, the reads see the end, and
;(stream-fault) gives what it was, so a reader can know it is not the end
;of what it reads. There is no throw in it
(defun fault-fill (state)
	(cond
		((first state) :lost)
		(:t (elem-set state 0 :t) "ab")))
(defun faulty () (lisp-stream `(lambda () (,fault-fill ',(list :nil)))))
(defq bad (faulty))
(assert-eq "read before the fault" "ab" (read-blk bad 2))
(assert-eq "no fault while there is more" :nil (stream-fault bad))
(assert-eq "read-blk at the fault is the end" :nil (read-blk bad 2))
(assert-eq "and the fault is what the fill gave" :lost (stream-fault bad))
(assert-eq "read-blk of more than there is has the part before the fault" "ab" (read-blk (setq bad (faulty)) 10))
(assert-eq "its fault" :lost (stream-fault bad))
(setq bad (faulty))
(assert-list-eq "read-char to the fault" (list (ascii-code "a") (ascii-code "b") :nil) (list (read-char bad) (read-char bad) (read-char bad)))
(assert-eq "read-char's fault" :lost (stream-fault bad))
(setq bad (faulty))
(assert-list-eq "read-line to the fault" (list "ab" :nil) (list (read-line bad) (read-line bad)))
(assert-eq "read-line's fault" :lost (stream-fault bad))
(defq seen (list) bad (faulty))
(lines! (# (push seen %0) :nil) bad)
(assert-list-eq "lines! to the fault" '("ab") seen)
(assert-eq "lines!'s fault" :lost (stream-fault bad))
(defq bad (lisp-stream '(lambda () :nil)))
(assert-eq "an end with no fault is :nil, as ever" :nil (read-blk bad 10))
(assert-eq "and has no fault" :nil (stream-fault bad))
(assert-eq "a stream of another kind has none" :nil (stream-fault (string-stream "x")))
