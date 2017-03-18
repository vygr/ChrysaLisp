(import 'class/sliceable/sliceable.inc)

(def-class 'vector 'sliceable)
(def-method 'create 'class/vector/create 'static nil '(r0))
(def-method 'new 'class/vector/new 'static nil '(r0))
(def-method 'init 'class/vector/init)
(def-method 'get_capacity 'class/vector/get_capacity 'static '(r0) '(r1))
(def-method 'set_capacity 'class/vector/set_capacity 'static '(r0 r1))
(def-method 'ref_back 'class/vector/ref_back 'static '(r0) '(r1))
(def-method 'get_back 'class/vector/get_back 'static '(r0) '(r1))
(def-method 'get_element 'class/vector/get_element 'static '(r0 r1) '(r1))
(def-method 'find 'class/vector/find 'static '(r0 r1) '(r1))
(def-method 'push_back 'class/vector/push_back 'static '(r0 r1))
(def-method 'pop_back 'class/vector/pop_back 'static '(r0))
(def-method 'clear 'class/vector/clear 'static '(r0))
(def-method 'for_each 'class/vector/for_each 'static '(r0 r1 r2 r3 r5) '(r1))
(def-method 'set_element 'class/vector/set_element 'static '(r0 r1 r2))
(def-method 'append 'class/vector/append 'static '(r0 r1 r2 r3))

(def-method 'deinit 'class/vector/deinit 'final)
(def-method 'get_length 'class/vector/get_length 'final)
(def-method 'ref_element 'class/vector/ref_element 'final)
(def-method 'slice 'class/vector/slice 'final)

(def-struct 'vector 'sliceable)
	(pptr 'vector_array)
	(uint 'vector_length)
	(uint 'vector_capacity)
	(ptr 'vector_e1)
	(ptr 'vector_e2)
	(ptr 'vector_e3)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/vector/get_capacity ()
	;inputs
	;r0 = vector object
	;outputs
	;r0 = vector object
	;r1 = vector capacity
	(vp-cpy-ir-ui r0 vector_capacity r1))

(defcfun class/vector/get_length ()
	;inputs
	;r0 = vector object
	;outputs
	;r0 = vector object
	;r1 = vector length
	(vp-cpy-ir-ui r0 vector_length r1))

(defcfun class/vector/get_element ()
	;inputs
	;r0 = vector object
	;r1 = vector element
	;outputs
	;r0 = vector object
	;r1 = object
	;trashes
	;r2
	(vp-cpy-ir r0 vector_array r2)
	(vp-shl-cr (pow2 ptr_size) r1)
	(vp-cpy-dr r2 r1 r1))
