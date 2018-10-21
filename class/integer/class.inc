(include 'class/obj/class.inc)

(def-class 'integer 'obj)
(dec-method 'vtable 'class/class_integer)
(dec-method 'create 'class/integer/create 'static '(r0) '(r0))
(dec-method 'new 'class/integer/new 'static nil '(r0))
(dec-method 'init 'class/integer/init 'static '(r0 r1 r2))
(dec-method 'get_value 'class/integer/get_value 'static '(r0) '(r0 r1))

(def-struct 'integer 'obj)
	(ptr 'value)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/integer/init ()
	;inputs
	;r0 = integer object (ptr)
	;r1 = vtable (pptr)
	;r2 = initial value (ptr)
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
	;r1 = value (ptr)
	(vp-cpy-ir r0 integer_value r1))
