;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mdct, the transform of the coded sound of films back to samples, and
; its windows, as AAC and AC-3 have them
;
; The IMDCT of n lines is 2n samples: their DCT-IV, by an FFT of n/2 in
; native code, lib/media/lisp.vp media_dct4, unfolded. (mdct-tables n)
; makes what the DCT-IV of n takes, (mdct-imdct dct4 tables lines n) the
; 2n samples, by 2/N as the transform is defined. (mdct-sine n) and
; (mdct-kbd n alpha) are the rising halves of the sine window of 2n and
; the Kaiser-Bessel derived one. The native is taken where it is used,
; (ffi "lib/media/media_dct4").
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;pi, as a real, by Newton's steps for tan p = 0 from 3, kept where the
;functions find it as they run
(defq +mdct (Emap) pi (n2r 3))
(times 4 (setq pi (- pi (/ (sin pi) (cos pi)))))
(. +mdct :insert :pi pi)

(defun mdct-pi ()
	; (mdct-pi) -> real
	(. +mdct :find :pi))

(defun mdct-tables (n)
	; (mdct-tables n) -> (tw rev)
	;what media_dct4 takes for a DCT-IV of n: the twiddles before,
	;e^(-i pi (4j+1)/4n), after, e^(-i pi k/n), of the FFT of m, n/2,
	;e^(-2 pi i k/m), each cos and sin, and the bit reversal of m
	(defq m (>> n 1) tw (cap (* 5 m) (reals)) rev (cap m (nums)) bits 0 pi (. +mdct :find :pi)
		twiddle (lambda (a) (push tw (cos a) (sin a))))
	(each (# (twiddle (/ (* (neg pi) (n2r (inc (* 4 %0)))) (n2r (* 4 n))))) (range 0 m))
	(each (# (twiddle (/ (* (neg pi) (n2r %0)) (n2r n)))) (range 0 m))
	(each (# (twiddle (/ (* (n2r -2) pi (n2r %0)) (n2r m)))) (range 0 (>> m 1)))
	(while (< (<< 1 bits) m) (setq bits (inc bits)))
	(each (lambda (i)
			(defq r 0)
			(each (# (if (/= 0 (logand i (<< 1 %0))) (setq r (logior r (<< 1 (- bits 1 %0)))))) (range 0 bits))
			(push rev r))
		(range 0 m))
	(list tw rev))

(defun mdct-imdct (dct4 tables lines n)
	; (mdct-imdct dct4 tables lines n) -> reals
	;the 2n samples of the IMDCT of n lines: their DCT-IV y, by 2/N, 1/n,
	;as y from n/2, then -y backwards, then -y to n/2
	(defq y (nums-scale (dct4 lines (first tables) (second tables) (cat lines)) (/ (n2r 1) (n2r n)))
		neg (nums-scale y (n2r -1)))
	(cat (slice y (>> n 1) n) (reverse neg) (slice neg 0 (>> n 1))))

(defun mdct-i0 (x)
	; (mdct-i0 x) -> real
	;the Bessel function I0, its series, the terms till they are nothing
	(defq sum (n2r 1) term (n2r 1) k 1 q (/ (* x x) (n2r 4)))
	(while (> (* term (n2r 100000000000000000)) sum)
		(setq term (/ (* term q) (n2r (* k k))) sum (+ sum term) k (inc k)))
	sum)

(defun mdct-sine (n)
	; (mdct-sine n) -> reals
	;the rising half, n, of the sine window of 2n
	(defq out (cap n (reals)) pi (. +mdct :find :pi))
	(each (# (push out (sin (/ (* pi (+ (n2r %0) (n2r 0.5))) (n2r (* 2 n)))))) (range 0 n))
	out)

(defun mdct-kbd (n alpha)
	; (mdct-kbd n alpha) -> reals
	;the rising half, n, of the Kaiser-Bessel derived window of 2n
	(defq out (cap n (reals)) sum (n2r 0) pi (. +mdct :find :pi)
		kernel (map (lambda (j)
				(defq t (- (/ (* (n2r 2) (n2r j)) (n2r n)) (n2r 1)))
				(mdct-i0 (* pi (n2r alpha) (sqrt (- (n2r 1) (* t t))))))
			(range 0 (inc n))))
	(defq total (reduce (const +) kernel (n2r 0)))
	(each (lambda (j)
			(setq sum (+ sum (elem-get kernel j)))
			(push out (sqrt (/ sum total))))
		(range 0 n))
	out)

;module
(export-symbols '(mdct-pi mdct-tables mdct-imdct mdct-sine mdct-kbd))
(env-pop)
