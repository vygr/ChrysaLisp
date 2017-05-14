(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;
; math static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_math)
(def-method 'random 'sys/math_random 'static '(r0) '(r0))
(def-method 'isqrt 'sys/math_isqrt 'static '(r0) '(r0))
(def-method 'fsqrt 'sys/math_fsqrt 'static '(r0) '(r0))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun sys/math_fsqrt ()
	;inputs
	;r0 = 16.16 number
	;outputs
	;r0 = 16.16 sqrt
	;trashes
	;r1-r3
	(vp-shl-cr 16 r0)
	(f-call 'sys_math 'isqrt '(r0) '(r0)))
