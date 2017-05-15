(import 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;
; math static class
;;;;;;;;;;;;;;;;;;;

(def-class 'math)
(def-method 'random 'sys/math/random 'static '(r0) '(r0))
(def-method 'frac 'sys/math/frac 'static '(r0) '(r0))
(def-method 'floor 'sys/math/floor 'static '(r0) '(r0))
(def-method 'fmod 'sys/math/fmod 'static '(r0 r1) '(r0))
(def-method 'isqrt 'sys/math/isqrt 'static '(r0) '(r0))
(def-method 'fsqrt 'sys/math/fsqrt 'static '(r0) '(r0))
(def-method 'fsin 'sys/math/fsin 'static '(r0) '(r0))
(def-method 'fcos 'sys/math/fcos 'static '(r0) '(r0))

(defcvar 'fp_shift 16 'fp_2pi 411774 'fp_pi 205887 'fp_hpi 102943 'fp_qpi 51471 'fp_rpi 20861
	'fp_int_mask (bit-shl -1 fp_shift) 'fp_frac_mask (bit-not fp_int_mask))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun sys/math/fsqrt ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 sqrt
	;trashes
	;r1-r3
	(vp-shl-cr fp_shift r0)
	(f-call 'math 'isqrt '(r0) '(r0)))

(defcfun sys/math/frac ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 fraction
	;trashes
	;r1
	(vpif '(r0 >= 0))
		(vp-and-cr fp_frac_mask r0)
	(else)
		(vp-cpy-rr r0 r1)
		(vp-cpy-cr 1.0 r0)
		(vp-and-cr fp_frac_mask r1)
		(vp-sub-rr r1 r0)
	(endif))

(defcfun sys/math/floor ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 floor
	;trashes
	;r1
	(vpif '(r0 >= 0))
		(vp-and-cr fp_int_mask r0)
	(else)
		(vp-cpy-rr r0 r1)
		(vp-and-cr fp_frac_mask r1)
		(vp-sub-rr r1 r0)
	(endif))
