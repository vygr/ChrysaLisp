(include 'class/obj/class.inc)

(def-class 'integer 'obj)
(dec-method 'vtable 'class/integer/vtable)
(dec-method 'create 'class/integer/create 'static '(r0) '(r0))
(dec-method 'init 'class/integer/init 'static '(r0 r1 r2))
(dec-method 'get_value 'class/integer/get_value 'static '(r0) '(r0 r1))

(dec-method 'lisp_add 'class/integer/lisp_add 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_sub 'class/integer/lisp_sub 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_mul 'class/integer/lisp_mul 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_div 'class/integer/lisp_div 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_mod 'class/integer/lisp_mod 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fmul 'class/integer/lisp_fmul 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fdiv 'class/integer/lisp_fdiv 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fmod 'class/integer/lisp_fmod 'static '(r0 r1) '(r0 r1))

(dec-method 'lisp_band 'class/integer/lisp_band 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_bor 'class/integer/lisp_bor 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_bxor 'class/integer/lisp_bxor 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_bshr 'class/integer/lisp_bshr 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_basr 'class/integer/lisp_basr 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_bshl 'class/integer/lisp_bshl 'static '(r0 r1) '(r0 r1))

(dec-method 'lisp_min 'class/integer/lisp_min 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_max 'class/integer/lisp_max 'static '(r0 r1) '(r0 r1))

(dec-method 'lisp_eq 'class/integer/lisp_eq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_ne 'class/integer/lisp_ne 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_lt 'class/integer/lisp_lt 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gt 'class/integer/lisp_gt 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_le 'class/integer/lisp_le 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_ge 'class/integer/lisp_ge 'static '(r0 r1) '(r0 r1))

(def-struct 'integer 'obj)
	(long 'value)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/integer/init ()
	;inputs
	;r0 = integer object (ptr)
	;r1 = vtable (pptr)
	;r2 = initial value (long)
	;outputs
	;r0 = integer object (ptr)
	;r1 = 0 if error, else ok
	(vp-cpy-ri r2 r0 integer_value)
	(s-call 'integer 'init '(r0 r1) '(r0 r1)))

(defcfun class/integer/get_value ()
	;inputs
	;r0 = integer object (ptr)
	;outputs
	;r0 = integer object (ptr)
	;r1 = value (long)
	(vp-cpy-ir r0 integer_value r1))
