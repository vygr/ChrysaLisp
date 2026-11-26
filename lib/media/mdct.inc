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

(import "lib/consts/numbers.inc")

;module
(env-push)

;pi, as a double, by Newton's steps for tan p = 0 from 3, kept where the
;functions find it as they run
(defq +mdct (Emap) pi (n2d 3))
(times 4 (setq pi (- pi (/ (sin pi) (cos pi)))))
(. +mdct :insert :pi pi)

;where its series is ended, a term this much less than the sum
(defq +mdct_series_end (n2d 100000000000000000))

(defun mdct-pi ()
	; (mdct-pi) -> double
	(. +mdct :find :pi))

(defun mdct-tables (n)
	; (mdct-tables n) -> (tw rev)
	;what media_dct4 takes for a DCT-IV of n: the twiddles before,
	;e^(-i pi (4j+1)/4n), after, e^(-i pi k/n), of the FFT of m, n/2,
	;e^(-2 pi i k/m), each cos and sin, and the bit reversal of m
	(defq m (>> n 1) tw (cap (* 5 m) (doubles)) rev (cap m (nums)) bits 0 pi (. +mdct :find :pi)
		twiddle (lambda (a) (push tw (cos a) (sin a))))
	(each! (# (twiddle (/ (* (neg pi) (n2d (inc (* 4 (!))))) (n2d (* 4 n))))) '() 0 m)
	(each! (# (twiddle (/ (* (neg pi) (n2d (!))) (n2d n)))) '() 0 m)
	(each! (# (twiddle (/ (* +double_-2 pi (n2d (!))) (n2d m)))) '() 0 (>> m 1))
	(while (< (<< 1 bits) m) (setq bits (inc bits)))
	(each! (lambda () (defq i (!)) (defq r 0)
			(each! (# (if (/= 0 (logand i (<< 1 (!)))) (setq r (logior r (<< 1 (- bits 1 (!))))))) '() 0 bits)
			(push rev r)) '() 0 m)
	(list tw rev))

(defun mdct-imdct (dct4 tables lines n)
	; (mdct-imdct dct4 tables lines n) -> doubles
	;the 2n samples of the IMDCT of n lines: their DCT-IV y, by 2/N, 1/n,
	;as y from n/2, then -y backwards, then -y to n/2
	(defq y (nums-scale (dct4 lines (first tables) (second tables) (cat lines)) (/ +double_1 (n2d n)))
		neg (nums-scale y +double_-1))
	(cat (slice y (>> n 1) n) (reverse neg) (slice neg 0 (>> n 1))))

(defun mdct-i0 (x)
	; (mdct-i0 x) -> double
	;the Bessel function I0, its series, the terms till they are nothing
	(defq sum +double_1 term +double_1 k 1 q (/ (* x x) +double_4))
	(while (> (* term +mdct_series_end) sum)
		(setq term (/ (* term q) (n2d (* k k))) sum (+ sum term) k (inc k)))
	sum)

(defun mdct-sine (n)
	; (mdct-sine n) -> doubles
	;the rising half, n, of the sine window of 2n
	(defq pi (. +mdct :find :pi))
	(reduce! (# (push %0 (sin (/ (* pi (+ (n2d (!)) +double_1/2)) (n2d (* 2 n)))))) '() (cap n (doubles)) 0 n))

(defun mdct-kbd (n alpha)
	; (mdct-kbd n alpha) -> doubles
	;the rising half, n, of the Kaiser-Bessel derived window of 2n
	(defq out (cap n (doubles)) sum +double_0 pi (. +mdct :find :pi)
		kernel (map! (lambda () (defq t (- (/ (* +double_2 (n2d (!))) (n2d n)) +double_1))
				(mdct-i0 (* pi (n2d alpha) (sqrt (- +double_1 (* t t)))))) '() 0 (inc n)))
	(defq total (reduce (const +) kernel +double_0))
	(each! (lambda (e) (setq sum (+ sum e))
			(push out (sqrt (/ sum total)))) (list kernel) 0 n)
	out)

;module
(export-symbols '(mdct-pi mdct-tables mdct-imdct mdct-sine mdct-kbd))
(env-pop)
