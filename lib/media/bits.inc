;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bits, of a str, high bit first, as the coded sound of films has them
;
; A reader is (str (nums at)), at in bits from the first, a raw number
; the native reads and writes in place. (bits-of str) makes one, the str
; with bytes enough after it to read past its end. (bits-get reader n)
; the next n bits, 0 to 25, (bits-1 reader) the next, both native,
; (bits-skip reader n) steps over n, (bits-at reader) where it is,
; (bits-seek reader at) puts it there, (bits-str reader) its str.
; (bits-huff reader table) the next code of a Huffman table made by
; lib/media/huff.inc, what it stands for, native too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(ffi (path-to-absolute "./media_bits_get") bits-get)
; (bits-get reader n) -> num
(ffi (path-to-absolute "./media_bits_1") bits-1)
; (bits-1 reader) -> 0 | 1
(ffi (path-to-absolute "./media_huff") media-huff)

(defun bits-of (s)
	; (bits-of str) -> reader
	(list (cat s (char 0 8)) (nums 0)))

(defun bits-skip (in n)
	; (bits-skip reader n) -> reader
	(defq at (second in))
	(elem-set at 0 (+ (first at) n))
	in)

(defun bits-at (in)
	; (bits-at reader) -> num
	(first (second in)))

(defun bits-seek (in at)
	; (bits-seek reader at) -> reader
	(elem-set (second in) 0 at)
	in)

(defun bits-str (in)
	; (bits-str reader) -> str
	(first in))

(defun bits-huff (in table)
	; (bits-huff reader table) -> index
	;the native gives where it ends in the low 32 bits and the index above
	(defq got (media-huff (first in) (first (second in)) table))
	(elem-set (second in) 0 (logand got 0xffffffff))
	(>> got 32))

;module
(export-symbols '(bits-of bits-get bits-1 bits-skip bits-at bits-seek bits-str bits-huff))
(env-pop)
