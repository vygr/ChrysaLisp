(import 'class/obj/class.inc)

(def-class 'hash_set 'obj)
(dec-method 'vtable 'class/class_hash_set)
(dec-method 'create 'class/hash_set/create 'static '(r0 r1) '(r0))
(dec-method 'new 'class/hash_set/new 'static nil '(r0))
(dec-method 'init 'class/hash_set/init 'static '(r0 r1 r2 r3))
(dec-method 'num_buckets 'class/hash_set/num_buckets 'static '(r0) '(r0 r1))
(dec-method 'get_bucket 'class/hash_set/get_bucket 'static '(r0 r1) '(r0 r1))
(dec-method 'clear 'class/hash_set/clear 'static '(r0) '(r0))
(dec-method 'for_each 'class/hash_set/for_each 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'find 'class/hash_set/find 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'insert 'class/hash_set/insert 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'erase 'class/hash_set/erase 'static '(r0 r1 r2) '(r0))
(dec-method 'copy 'class/hash_set/copy 'static '(r0 r1) '(r0 r1))
(dec-method 'get_iters 'class/hash_set/get_iters 'static '(r0) '(r0 r1 r2))
(dec-method 'key_callback 'class/obj/null 'static '(r0 r1) '(r0 r1))

(dec-method 'deinit 'class/hash_set/deinit 'override)

(def-struct 'hash_set 'obj)
	(uint 'num_buckets)
	(ptr 'buckets)
	(ptr 'key_callback)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/hash_set/num_buckets ()
	;inputs
	;r0 = hash_set object (ptr)
	;outputs
	;r0 = hash_set object (ptr)
	;r1 = num buckets (uint)
	(vp-cpy-ir-ui r0 hash_set_num_buckets r1))
