(import 'class/sequence/sequence.inc)

(def-class 'array 'sequence)
(def-method 'create 'class/array/create 'static nil '(r0))
(def-method 'new 'class/array/new 'static nil '(r0))
(def-method 'init 'class/array/init)
(def-method 'get_capacity 'class/array/get_capacity 'static '(r0) '(r0 r1))
(def-method 'set_capacity 'class/array/set_capacity 'static '(r0 r1)'(r0))
(def-method 'set_length 'class/array/set_length 'static '(r0 r1) '(r0))
(def-method 'find 'class/array/find 'static '(r0 r1) '(r0 r1))
(def-method 'for_each 'class/array/for_each 'static '(r0 r1 r2 r3 r4) '(r0 r1))
(def-method 'sort 'class/array/sort 'static '(r0 r1 r2 r3 r4 r5) '(r0))
(def-method 'partition 'class/array/partition 'static '(r0 r1 r2 r3 r4) '(r0 r1))
(def-method 'get_back 'class/array/get_back 'static '(r0) '(r0 r1))
(def-method 'get_first 'class/array/get_first 'static '(r0) '(r0 r1))
(def-method 'get_second 'class/array/get_second 'static '(r0) '(r0 r1))
(def-method 'get_third 'class/array/get_third 'static '(r0) '(r0 r1))
(def-method 'get_forth 'class/array/get_forth 'static '(r0) '(r0 r1))
(def-method 'get_element 'class/array/get_element 'static '(r0 r1) '(r0 r1))
(def-method 'push_back 'class/array/push_back 'static '(r0 r1) '(r0))
(def-method 'get_iter 'class/array/get_iter 'static '(r0 r1) '(r0 r1))
(def-method 'get_iters 'class/array/get_iters 'static '(r0) '(r0 r1 r2))
(def-method 'get_begin 'class/array/get_begin 'static '(r0) '(r0 r1))
(def-method 'get_end 'class/array/get_end 'static '(r0) '(r0 r1))
(def-method 'each_callback 'class/obj/null 'static '(r0 r1) '(r1))
(def-method 'sort_callback 'class/obj/null 'static '(r0 r1 r2) '(r0))

(def-method 'clear 'class/array/clear 'virtual '(r0) '(r0))
(def-method 'ref_back 'class/array/ref_back 'virtual '(r0) '(r0 r1))
(def-method 'pop_back 'class/array/pop_back 'virtual '(r0) '(r0))
(def-method 'set_element 'class/array/set_element 'virtual '(r0 r1 r2) '(r0))
(def-method 'append 'class/array/append 'virtual '(r0 r1 r2 r3) '(r0))

(def-method 'deinit 'class/array/deinit 'override)
(def-method 'get_length 'class/array/get_length 'override)
(def-method 'ref_element 'class/array/ref_element 'override)
(def-method 'slice 'class/array/slice 'override)

(def-struct 'array 'sequence)
	(pulong 'array)
	(uint 'length)
	(uint 'capacity)
	(ulong 'e0)
	(ulong 'e1)
	(ulong 'e2)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/array/init ()
	;inputs
	;r0 = array object (ptr)
	;r1 = vtable (pptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = 0 if error, else ok
	;trashes
	;all but r0
	(vp-xor-rr r2 r2)
	(vp-cpy-ri-i r2 r0 array_length)
	(vp-cpy-cr (bit-shr (sub array_size array_e0) (log2 long_size)) r2)
	(vp-cpy-ri-i r2 r0 array_capacity)
	(vp-lea-i r0 array_e0 r2)
	(vp-cpy-ri r2 r0 array_array)
	(s-call 'array 'init '(r0 r1) '(r0 r1)))

(defcfun class/array/clear ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;trashes
	;r1
	(vp-xor-rr r1 r1)
	(vp-cpy-ri-i r1 r0 array_length))

(defcfun class/array/get_capacity ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = capacity (uint)
	(vp-cpy-ir-ui r0 array_capacity r1))

(defcfun class/array/get_length ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = length (uint)
	(vp-cpy-ir-ui r0 array_length r1))

(defcfun class/array/set_length ()
	;inputs
	;r0 = array object (ptr)
	;r1 = length (uint)
	;outputs
	;r0 = array object (ptr)
	(vp-cpy-ri-i r1 r0 array_length))

(defcfun class/array/get_first ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	(vp-cpy-ir r0 array_array r1)
	(vp-cpy-ir r1 0 r1))

(defcfun class/array/get_second ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	(vp-cpy-ir r0 array_array r1)
	(vp-cpy-ir r1 long_size r1))

(defcfun class/array/get_third ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	(vp-cpy-ir r0 array_array r1)
	(vp-cpy-ir r1 (mul long_size 2) r1))

(defcfun class/array/get_forth ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	(vp-cpy-ir r0 array_array r1)
	(vp-cpy-ir r1 (mul long_size 3) r1))

(defcfun class/array/get_element ()
	;inputs
	;r0 = array object (ptr)
	;r1 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;trashes
	;r2
	(vp-cpy-ir r0 array_array r2)
	(vp-shl-cr (log2 long_size) r1)
	(vp-cpy-dr r2 r1 r1))

(defcfun class/array/get_iter ()
	;inputs
	;r0 = array object (ptr)
	;r1 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;r1 = element iter (plong)
	;trashes
	;r2
	(vp-cpy-ir r0 array_array r2)
	(vp-shl-cr (log2 long_size) r1)
	(vp-add-rr r2 r1))

(defcfun class/array/get_iters ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = begin element iter (plong)
	;r2 = end element iter (plong)
	(vp-cpy-ir-ui r0 array_length r2)
	(vp-cpy-ir r0 array_array r1)
	(vp-shl-cr (log2 long_size) r2)
	(vp-add-rr r1 r2))

(defcfun class/array/get_begin ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = begin element iter (plong)
	(vp-cpy-ir r0 array_array r1))

(defcfun class/array/get_end ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;r1 = end element iter (plong)
	;trashes
	;r2
	(f-call 'array 'get_iter '(r0 (r0 array_length ui))))

(defcfun class/array/pop_back ()
	;inputs
	;r0 = array object (ptr)
	;outputs
	;r0 = array object (ptr)
	;trashes
	;r1
	(vp-cpy-ir-ui r0 array_length r1)
	(vp-sub-cr 1 r1)
	(vp-cpy-ri-i r1 r0 array_length))

(defcfun class/array/set_element ()
	;inputs
	;r0 = array object (ptr)
	;r1 = element (long)
	;r2 = element index (uint)
	;outputs
	;r0 = array object (ptr)
	;trashes
	;r2-r3
	(vp-cpy-ir r0 array_array r3)
	(vp-shl-cr (log2 long_size) r2)
	(vp-cpy-rd r1 r3 r2))
