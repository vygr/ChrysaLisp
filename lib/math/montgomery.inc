;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Montgomery's multiply, mod an odd number m, of the big numbers of
; lib/math/bignum.inc
;
; A number x is worked as x R mod m, R 2 to the 26 times as many limbs as m
; has, its Montgomery form. The product of two of those over R is the form
; of their product, and is made by the native bn_montmul,
; lib/math/lisp_bignum.vp, a row of limbs at a time with no division, a
; fraction of the time of (bn-mulmod). Adding and taking away are as they
; are of any number mod m, (bn-addmod) and (bn-submod), given the mont, as
; m is its first. What the curves of ECDSA are worked with.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./bignum.inc")

(ffi "lib/math/bn_montmul" bn-montmul)
; (bn-montmul out a b m minv) -> out

;module
(env-push)

(defq +mont_bits 26 +mont_mask (dec (<< 1 26)))

(defun bn-mont (m)
	; (bn-mont m) -> mont
	;what numbers are worked with mod m, m odd: m, its limbs, -1 over m mod
	;2 to the 26, R mod m, the form of 1, R squared mod m, and m as
	;(bn-mod) has it
	(setq m (bn-trim m))
	(defq n (length m) m0 (first m) inv 1 modulus (bn-modulus m)
		r (bn-mod (bn-shl (bn-from-num 1) (* +mont_bits n)) modulus))
	;1 over m by Newton's step, each doubling the bits that are right
	(times 5 (setq inv (logand (* inv (- 2 (* m0 inv))) +mont_mask)))
	(list m n (logand (neg inv) +mont_mask) r (bn-mod (bn-mul r r) modulus) modulus))

(defun bn-mul-mont (a b mont)
	; (bn-mul-mont a b mont) -> nums
	;a times b over R, mod m, each less than m: of two forms the form of
	;their product
	(bind '(m n minv &ignore) mont)
	(bn-montmul (bn-zeros (inc n)) (bn-pad a n) (bn-pad b n) m minv))

(defun bn-to-mont (x mont)
	; (bn-to-mont x mont) -> nums
	;the form of x, x R mod m
	(bn-mul-mont (bn-mod x (elem-get mont 5)) (elem-get mont 4) mont))

(defun bn-from-mont (x mont)
	; (bn-from-mont x mont) -> nums
	;the number of a form
	(bn-trim (bn-mul-mont x (bn-from-num 1) mont)))

(defun bn-pow-mont (a e mont)
	; (bn-pow-mont a e mont) -> nums
	;the form of a to the power e, a a form, e a big number, from its top bit
	;down, each giving way to the other tasks of the node
	(defq out (elem-get mont 3) i (bn-bits e))
	(while (> i 0)
		(task-slice)
		(-- i)
		(setq out (bn-mul-mont out out mont))
		(if (= (bn-bit e i) 1) (setq out (bn-mul-mont out a mont))))
	out)

(defun bn-inv-mont (a mont)
	; (bn-inv-mont a mont) -> nums
	;the form of 1 over a, a a form, m a prime: a to the prime less 2
	(bn-pow-mont a (bn-sub (first mont) (bn-from-num 2)) mont))

;module
(export-symbols '(bn-mont bn-mul-mont bn-to-mont bn-from-mont bn-pow-mont bn-inv-mont))
(env-pop)
