(import 'class/ref/ref.inc)

(def-class 'boxed_ptr 'ref)
(def-method 'create 'class/boxed_ptr/create 'static '(r0) '(r0))
(def-method 'new 'class/boxed_ptr/new 'static nil '(r0))
(def-method 'init 'class/boxed_ptr/init 'static '(r0 r1 r2))
(def-method 'get_value 'class/boxed_ptr/get_value 'static '(r0) '(r0 r1))

(def-struct 'boxed_ptr 'ref)
	(ptr 'value)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/boxed_ptr/init ()
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;r2 = initial value
	;outputs
	;r0 = object
	;r1 = 0 if error, else ok
	(vp-cpy-ri r2 r0 boxed_ptr_value)
	(s-call 'boxed_ptr 'init '(r0 r1) '(r0 r1)))

(defcfun class/boxed_ptr/get_value ()
	;inputs
	;r0 = object
	;outputs
	;r0 = object
	;r1 = value
	(vp-cpy-ir r0 boxed_ptr_value r1))
