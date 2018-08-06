(import 'class/obj/obj.inc)

(def-class 'function 'obj)
(def-method 'vtable 'class/class_function)
(def-method 'create 'class/function/create 'static '(r0) '(r0))
(def-method 'new 'class/function/new 'static nil '(r0))
(def-method 'init 'class/function/init 'static '(r0 r1 r2))
(def-method 'get_value 'class/function/get_value 'static '(r0) '(r0 r1))

(def-struct 'function 'obj)
	(ptr 'value)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/function/init ()
	;inputs
	;r0 = function object (ptr)
	;r1 = vtable (pptr)
	;r2 = initial value (ptr)
	;outputs
	;r0 = function object (ptr)
	;r1 = 0 if error, else ok
	(vp-cpy-ri r2 r0 function_value)
	(s-call 'function 'init '(r0 r1) '(r0 r1)))

(defcfun class/function/get_value ()
	;inputs
	;r0 = function object (ptr)
	;outputs
	;r0 = function object (ptr)
	;r1 = value (ptr)
	(vp-cpy-ir r0 function_value r1))
