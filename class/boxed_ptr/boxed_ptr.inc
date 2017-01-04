(import 'class/ref/ref.inc)

(def-class 'boxed_ptr 'ref)
(def-method 'create 'class/boxed_ptr/create)
(def-method 'new 'class/boxed_ptr/new)
(def-method 'init 'class/boxed_ptr/init)
(def-method 'get_value 'class/boxed_ptr/get_value 'static '(r0) '(r1))
(def-method 'set_value 'class/boxed_ptr/set_value 'static '(r0 r1))
(def-method 'get_flags 'class/boxed_ptr/get_flags 'static '(r0) '(r1))
(def-method 'set_flags 'class/boxed_ptr/set_flags 'static '(r0 r1))

(def-struct 'boxed_ptr 'ref)
	(ptr 'boxed_ptr_value)
	(ulong 'boxed_ptr_flags)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/boxed_ptr/get_flags ()
	;inputs
	;r0 = object
	;outputs
	;r0 = object
	;r1 = value
	(vp-cpy-ir r0 boxed_ptr_flags r1))

(defcfun class/boxed_ptr/get_value ()
	;inputs
	;r0 = object
	;outputs
	;r0 = object
	;r1 = value
	(vp-cpy-ir r0 boxed_ptr_value r1))

(defcfun class/boxed_ptr/set_flags ()
	;inputs
	;r0 = object
	;r1 = value
	;outputs
	;r0 = object
	(vp-cpy-ri r1 r0 boxed_ptr_flags))

(defcfun class/boxed_ptr/set_value ()
	;inputs
	;r0 = object
	;r1 = value
	;outputs
	;r0 = object
	(vp-cpy-ri r1 r0 boxed_ptr_value))
