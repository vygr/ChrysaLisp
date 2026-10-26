(report-header "Streams: huff, a Huffman book as a table, each code read back by native code")

(import "lib/media/huff.inc")
(import "lib/media/aac_tables.inc")

(defq hf_read (ffi "lib/media/media_huff"))

(defun hf-code (n v shift)
	;a code of n bits, v, shift bits in, the rest of the bytes 1s, and 32
	;bytes more as lib/media/bits.inc puts after a str, of 1s here
	(defq bits (+ shift n) bytes (/ (+ bits 7) 8)
		;the code high in the bits, 1s before and after it
		word (logior (<< (dec (<< 1 shift)) (- (* bytes 8) shift))
			(<< v (- (* bytes 8) bits))
			(dec (<< 1 (- (* bytes 8) bits)))))
	(cat (apply (const cat) (map (# (char (logand (>> word (* 8 (- bytes 1 %0))) 0xff))) (range 0 bytes)))
		(const (cat (char -1 8) (char -1 8) (char -1 8) (char -1 8)))))

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

;a reader read past the end of its str, or before its start, as a corrupt
;stream may make it, reads 0s, and not what is past the str
(import "lib/media/bits.inc")
(defq hf_in (bits-of (char -1 2)) hf_table (huff-table *aac_sf_book*)
	hf_z (bits-of (char 0 8)) hf_zero (bits-huff hf_z hf_table))
(assert-eq "bits of the str" 0xffff (bits-get hf_in 16))
(assert-eq "bits in the 0s after it" 0 (bits-get hf_in 16))
(assert-eq "bits far past it" 0 (bits-get (bits-seek hf_in 1000000) 25))
(assert-eq "and the place moved past them" 1000025 (bits-at hf_in))
(assert-eq "bits before it" 0 (bits-get (bits-seek hf_in -1000) 8))
(assert-eq "a code far past it is a code of 0s" hf_zero (bits-huff (bits-seek hf_in 1000000) hf_table))
(assert-eq "and the place moved past it" (+ 1000000 (bits-at hf_z)) (bits-at hf_in))
(undef (env) 'hf_in 'hf_table 'hf_zero 'hf_z)
