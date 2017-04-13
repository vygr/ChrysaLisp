(import 'class/sliceable/sliceable.inc)

(def-class 'vector 'sliceable)
(def-method 'create 'class/vector/create 'static nil '(r0))
(def-method 'new 'class/vector/new 'static nil '(r0))
(def-method 'init 'class/vector/init)
(def-method 'get_capacity 'class/vector/get_capacity 'static '(r0) '(r0 r1))
(def-method 'set_capacity 'class/vector/set_capacity 'static '(r0 r1)'(r0))
(def-method 'ref_back 'class/vector/ref_back 'static '(r0) '(r0 r1))
(def-method 'get_back 'class/vector/get_back 'static '(r0) '(r0 r1))
(def-method 'get_element 'class/vector/get_element 'static '(r0 r1) '(r0 r1))
(def-method 'find 'class/vector/find 'static '(r0 r1) '(r0 r1))
(def-method 'push_back 'class/vector/push_back 'static '(r0 r1) '(r0))
(def-method 'pop_back 'class/vector/pop_back 'static '(r0) '(r0))
(def-method 'clear 'class/vector/clear 'static '(r0) '(r0))
(def-method 'for_each 'class/vector/for_each 'static '(r0 r1 r2 r3 r5) '(r0 r1))
(def-method 'set_element 'class/vector/set_element 'static '(r0 r1 r2)'(r0))
(def-method 'append 'class/vector/append 'static '(r0 r1 r2 r3) '(r0))
(def-method 'get_iter 'class/vector/get_iter 'static '(r0 r1) '(r0 r1))
(def-method 'callback 'class/obj/null 'static '(r0 r1) '(r1))

(def-method 'deinit 'class/vector/deinit 'final)
(def-method 'get_length 'class/vector/get_length 'final)
(def-method 'ref_element 'class/vector/ref_element 'final)
(def-method 'slice 'class/vector/slice 'final)

(def-struct 'vector 'sliceable)
	(pptr 'array)
	(uint 'length)
	(uint 'capacity)
	(ptr 'e0)
	(ptr 'e1)
	(ptr 'e2)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/vector/init ()
	;inputs
	;r0 = vector object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok
	;trashes
	;all but r0, r4
	(vp-xor-rr r2 r2)
	(vp-cpy-ri-i r2 r0 vector_length)
	(vp-cpy-cr (bit-shr (sub vector_size vector_e0) (pow2 ptr_size)) r2)
	(vp-cpy-ri-i r2 r0 vector_capacity)
	(vp-lea-i r0 vector_e0 r2)
	(vp-cpy-ri r2 r0 vector_array)
	(s-call 'vector 'init '(r0 r1) '(_ r1)))

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

(defcfun class/vector/get_iter ()
	;inputs
	;r0 = vector object
	;r1 = vector element
	;outputs
	;r0 = vector object
	;r1 = iter pointer
	;trashes
	;r2
	(vp-cpy-ir r0 vector_array r2)
	(vp-shl-cr (pow2 ptr_size) r1)
	(vp-add-rr r2 r1))
