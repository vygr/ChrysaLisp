(report-header "Streams: Huffman, canonical codes of a limited length, in blocks or by a book")
(import "lib/streams/huffman.inc")

(defun hf-round (text &optional token_bits block)
	; text coded and back, and the size coded
	(defq coded (memory-stream) back (memory-stream))
	(huffman-compress (string-stream text) coded token_bits block)
	(stream-seek coded 0 0)
	(defq data (read-blk coded 1000000))
	(list (and (huffman-decompress (string-stream data) back)
			(cond
				((= (length text) 0) "")
				(:t (stream-seek back 0 0) (read-blk back (length text)))))
		(length data)))

;lengths: none over the limit, and as the sums of a code say, the sum of
;2^-length of each is 1 for a whole code
(defun hf-kraft (lengths)
	(defq most (reduce (const max) lengths 0))
	(reduce (# (if (> %1 0) (+ %0 (<< 1 (- most %1))) %0)) lengths 0))
(defq hf_fib (nums 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368))
(assert-list-eq "lengths of a few" '(1 2 3 3) (huffman-lengths (nums 10 5 2 1) 15))
(assert-eq "of none" 0 (reduce (const +) (huffman-lengths (nums 0 0 0) 15) 0))
(assert-list-eq "of one" '(0 1 0) (huffman-lengths (nums 0 7 0) 15))
(defq hf_long (huffman-lengths hf_fib 24) hf_short (huffman-lengths hf_fib 8))
(assert-eq "Fibonacci, the longest is as many as there are less one" 23 (reduce (const max) hf_long 0))
(assert-eq "limited to 8" 8 (reduce (const max) hf_short 0))
(assert-eq "and still a whole code" (<< 1 8) (hf-kraft hf_short))
(assert-eq "a whole code unlimited" (<< 1 23) (hf-kraft hf_long))

;the codes are canonical, as RFC 1951 3.2.2 has them, each the other way
;round to write: A 2, B 1, C 3, D 3, is B 0, A 10, C 110, D 111
(assert-list-eq "canonical codes" '(1 0 3 7) (huffman-codes (nums 2 1 3 3)))
(defq hf_bits (array 0 0) hf_ms (memory-stream) hf_lens (nums 2 1 3 3) hf_codes (huffman-codes hf_lens))
(each (# (write-bits hf_ms hf_bits (elem-get hf_codes %0) (elem-get hf_lens %0))) '(3 0 1 2 1))
(flush-bits hf_ms hf_bits)
(stream-seek hf_ms 0 0)
(setq hf_bits (array 0 0))
(defq hf_table (huffman-table hf_lens))
(assert-list-eq "and read back" '(3 0 1 2 1) (map (lambda (&) (huffman-symbol hf_ms hf_bits hf_table)) (range 0 5)))

(defq hf_fox "the quick brown fox jumps over the lazy dog")
(assert-eq "a stream there and back" hf_fox (first (hf-round hf_fox)))
(assert-eq "an empty one" "" (first (hf-round "")))
(assert-eq "one of a byte" "aaaaaaaaaaaaaaaa" (first (hf-round "aaaaaaaaaaaaaaaa")))
(defq hf_text (apply (const cat) (map (# (cat "line " (str %0) " of the text, ")) (range 0 2000))))
(bind '(hf_back hf_size) (hf-round hf_text 8 4096))
(assert-eq "blocks of 4096" hf_text hf_back)
(assert-true "and smaller" (< hf_size (/ (* (length hf_text) 3) 5)))
(assert-eq "tokens of 4 bits" hf_text (first (hf-round hf_text 4)))
(assert-eq "of 12" (slice hf_text 0 3000) (first (hf-round (slice hf_text 0 3000) 12 1000)))

;cut short, it says so
(defq hf_coded (memory-stream))
(huffman-compress (string-stream hf_text) hf_coded)
(stream-seek hf_coded 0 0)
(defq hf_cut (read-blk hf_coded 300))
(assert-eq "cut short" :nil (huffman-decompress (string-stream hf_cut) (memory-stream)))

;by a book, made of one text and used for another, kept and read again
(defq hf_book (huffman-book 8 (huffman-freqs (string-stream hf_text))) hf_kept (memory-stream))
(huffman-write-book hf_kept hf_book)
(stream-seek hf_kept 0 0)
(setq hf_book (huffman-read-book hf_kept))
(defq hf_coded (memory-stream) hf_back (memory-stream))
(assert-true "by a book, of a text with what the book's had not" (huffman-compress-static (string-stream hf_fox) hf_coded hf_book))
(stream-seek hf_coded 0 0)
(assert-true "and back" (huffman-decompress-static hf_coded hf_back hf_book))
(stream-seek hf_back 0 0)
(assert-eq "is the text" hf_fox (read-blk hf_back (length hf_fox)))
