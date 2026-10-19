;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bits, of a str, high bit first, as the coded sound of films has them
;
; A reader is (str at), at in bits from the first. (bits-of str) makes
; one, the str with bytes enough after it to read past its end.
; (bits-get reader n) the next n bits, 0 to 25, (bits-1 reader) the
; next, (bits-skip reader n) steps over n, (bits-at reader) where it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun bits-of (s)
	; (bits-of str) -> reader
	(list (cat s (char 0 8)) 0))

(defun bits-get (in n)
	; (bits-get reader n) -> num
	(defq at (second in) byte (>> at 3))
	(elem-set in 1 (+ at n))
	(logand (>> (code-be (first in) 4 byte) (- 32 (logand at 7) n)) (dec (<< 1 n))))

(defun bits-1 (in)
	; (bits-1 reader) -> 0 | 1
	(defq at (second in))
	(elem-set in 1 (inc at))
	(logand (>> (code (first in) 1 (>> at 3)) (- 7 (logand at 7))) 1))

(defun bits-skip (in n)
	; (bits-skip reader n) -> reader
	(elem-set in 1 (+ (second in) n))
	in)

(defun bits-at (in)
	; (bits-at reader) -> num
	(second in))

;module
(export-symbols '(bits-of bits-get bits-1 bits-skip bits-at))
(env-pop)
