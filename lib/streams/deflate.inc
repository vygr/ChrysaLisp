;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEFLATE, RFC 1951, and zlib round it, RFC 1950, a stream to a stream
;
; What PNG keeps its pixels in, what a .zip keeps its files in, and what a
; web server may send. A stage of a pipeline as lz4 and rle are: it reads
; one stream and writes another, and knows nothing of what either is.
;
; Inflate is done as zlib's own small one, puff, does it: a code of each
; length counted, the codes of a length one after the other, a code read a
; bit at a time, high bit first, from the low bits of each byte up. What was
; written is kept, the last 32KB, for a length and a distance back to copy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +inflate_window 32768
	;the base and extra bits of each length code from 257, and of each
	;distance code
	+inflate_len_base (nums 3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258)
	+inflate_len_extra (nums 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
	+inflate_dist_base (nums 1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073
		4097 6145 8193 12289 16385 24577)
	+inflate_dist_extra (nums 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13)
	;the order the lengths of the code length code come in
	+inflate_order (nums 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defun inflate-code (lengths)
	; (inflate-code lengths) -> (counts symbols)
	;a canonical code of these lengths of each symbol, 0 not used: how
	;many there are of each length, and the symbols in the order of their
	;codes
	(defq counts (nums 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) offsets (nums 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		symbols (map (lambda (_) 0) lengths))
	(each (# (elem-set counts %0 (inc (elem-get counts %0)))) lengths)
	(elem-set counts 0 0)
	;where the codes of each length begin, from 1, after those shorter
	(each (# (elem-set offsets (inc %0) (+ (elem-get offsets %0) (elem-get counts %0)))) (range 1 15))
	(each (lambda (len)
			(when (> len 0)
				(elem-set symbols (elem-get offsets len) (!))
				(elem-set offsets len (inc (elem-get offsets len)))))
		lengths)
	(list counts (apply (const nums) symbols)))

(defun inflate-symbol (in bits code)
	; (inflate-symbol in bits code) -> symbol | -1
	;the next symbol of a code, read a bit at a time
	(bind '(counts symbols) code)
	(defq at 0 lo 0 index 0 len 0 found -1)
	(while (and (< found 0) (< (++ len) 16))
		(defq bit (read-bits in bits 1))
		(cond
			((< bit 0) (setq len 16))
			(:t (setq at (logior at bit))
				(defq count (elem-get counts len))
				(cond
					((< (- at lo) count) (setq found (elem-get symbols (+ index (- at lo)))))
					(:t (setq index (+ index count) lo (<< (+ lo count) 1) at (<< at 1)))))))
	found)

(defun inflate-fixed ()
	; (inflate-fixed) -> (literals distances)
	;the codes of a block of fixed codes
	(list (inflate-code (cat (map (lambda (_) 8) (range 0 144)) (map (lambda (_) 9) (range 144 256))
			(map (lambda (_) 7) (range 256 280)) (map (lambda (_) 8) (range 280 288))))
		(inflate-code (map (lambda (_) 5) (range 0 30)))))

(defun inflate-dynamic (in bits)
	; (inflate-dynamic in bits) -> :nil | (literals distances)
	;the codes a block of its own codes says, at its head
	(defq nlen (+ (read-bits in bits 5) 257) ndist (+ (read-bits in bits 5) 1) ncode (+ (read-bits in bits 4) 4)
		lengths (map (lambda (_) 0) (range 0 19)))
	(each (# (elem-set lengths (elem-get +inflate_order %0) (read-bits in bits 3))) (range 0 ncode))
	(defq lencode (inflate-code lengths) all (list) ok :t)
	(while (and ok (< (length all) (+ nlen ndist)))
		(defq sym (inflate-symbol in bits lencode))
		(cond
			((< sym 0) (setq ok :nil))
			((< sym 16) (push all sym))
			(:t (defq previous (if (= sym 16) (if (empty? all) -1 (last all)) 0)
					repeat (case sym (16 (+ 3 (read-bits in bits 2))) (17 (+ 3 (read-bits in bits 3))) (:t (+ 11 (read-bits in bits 7)))))
				(if (< previous 0) (setq ok :nil) (times repeat (push all previous))))))
	(and ok (= (length all) (+ nlen ndist))
		(list (inflate-code (slice all 0 nlen)) (inflate-code (slice all nlen -1)))))

(defun inflate-decompress (in_stream out_stream &optional zlib)
	; (inflate-decompress in_stream out_stream [zlib]) -> :nil | :t
	;the bytes a DEFLATE stream is, raw, or with zlib's head and sum round
	;it. :nil if it is not one, or stops before its end, what was got by
	;then is written
	(defq bits (array 0 0) ring (apply (const nums) (map (lambda (_) 0) (range 0 +inflate_window)))
		at 0 ok :t last_block :nil fixed :nil)
	(defun emit (b)
		(write-char out_stream b)
		(elem-set ring at b)
		(setq at (logand (inc at) (const (dec +inflate_window)))))
	;zlib: a method of 8, deflate, and a check that the head is one
	(when zlib
		(defq cmf (read-bits in_stream bits 8) flg (read-bits in_stream bits 8))
		(setq ok (and (>= flg 0) (= (logand cmf 15) 8) (= 0 (% (+ (* cmf 256) flg) 31)) (not (bits? flg 0x20)))))
	(while (and ok (not last_block))
		(task-slice)
		(setq last_block (= (read-bits in_stream bits 1) 1))
		(defq kind (read-bits in_stream bits 2))
		(cond
			((= kind 0)
				;stored: from the next byte, a length, its complement, and that many
				(read-bits in_stream bits (logand (elem-get bits 1) 7))
				(defq len (read-bits in_stream bits 16) nlen (read-bits in_stream bits 16))
				(cond
					((/= (logxor len nlen) 0xffff) (setq ok :nil))
					(:t (times len (emit (read-bits in_stream bits 8))))))
			((or (= kind 1) (= kind 2))
				(defq codes (if (= kind 1) (ifn fixed (setq fixed (inflate-fixed))) (inflate-dynamic in_stream bits)))
				(cond
					((not codes) (setq ok :nil))
					(:t (bind '(literals distances) codes)
						(defq going :t)
						(while (and ok going)
							(defq sym (inflate-symbol in_stream bits literals))
							(cond
								((< sym 0) (setq ok :nil))
								((< sym 256) (emit sym))
								((= sym 256) (setq going :nil))
								((> sym 285) (setq ok :nil))
								(:t (defq i (- sym 257)
										len (+ (elem-get +inflate_len_base i) (read-bits in_stream bits (elem-get +inflate_len_extra i)))
										d (inflate-symbol in_stream bits distances))
									(cond
										((or (< d 0) (> d 29)) (setq ok :nil))
										(:t (defq back (+ (elem-get +inflate_dist_base d) (read-bits in_stream bits (elem-get +inflate_dist_extra d))))
											(times len (emit (elem-get ring (logand (- at back) (const (dec +inflate_window))))))))))))))
			(:t (setq ok :nil))))
	ok)

;module
(export-symbols '(inflate-decompress inflate-code inflate-symbol))
(env-pop)
