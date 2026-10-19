(report-header "Streams: huff, a Huffman book as a table, each code read back by native code")

(import "lib/media/huff.inc")
(import "lib/media/aac_tables.inc")

(defq hf_read (ffi "lib/media/media_huff"))

(defun hf-code (n v shift)
	;a code of n bits, v, shift bits in, the rest of the bytes 1s, and 4
	;bytes more as the reader wants
	(defq bits (+ shift n) bytes (/ (+ bits 7) 8)
		;the code high in the bits, 1s before and after it
		word (logior (<< (dec (<< 1 shift)) (- (* bytes 8) shift))
			(<< v (- (* bytes 8) bits))
			(dec (<< 1 (- (* bytes 8) bits)))))
	(cat (apply (const cat) (map (# (char (logand (>> word (* 8 (- bytes 1 %0))) 0xff))) (range 0 bytes)))
		(char -1 4)))

;every code of each of the books of AAC, at each of 8 places in a byte, reads
;back as what it stands for, its bits taken
(each (lambda (book name)
		(defq table (huff-table book) bad 0)
		(each (lambda ((n v))
				(defq sym (!))
				(each (lambda (shift)
						(defq got (hf_read (hf-code n v shift) shift table))
						(if (or (/= (>> got 32) sym) (/= (logand got 0xffffffff) (+ shift n))) (++ bad)))
					(range 0 8)))
			(partition book 2))
		(assert-eq (cat "every code of " name " at every place in a byte") 0 bad))
	(cat (list *aac_sf_book*) *aac_books*)
	'("the scale factors" "book 1" "book 2" "book 3" "book 4" "book 5" "book 6" "book 7" "book 8" "book 9" "book 10" "book 11"))
