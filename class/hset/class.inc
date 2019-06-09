(include 'class/obj/class.inc)

(def-class 'hset 'obj)
(dec-method 'vtable 'class/hset/vtable)
(dec-method 'create 'class/hset/create 'static '(r0 r1) '(r0))
(dec-method 'init 'class/hset/init 'static '(r0 r1 r2 r3))
(dec-method 'num_buckets 'class/hset/num_buckets 'static '(r0) '(r0 r1))
(dec-method 'get_bucket 'class/hset/get_bucket 'static '(r0 r1) '(r0 r1))
(dec-method 'clear 'class/hset/clear 'static '(r0) '(r0))
(dec-method 'for_each 'class/hset/for_each 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'find 'class/hset/find 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'insert 'class/hset/insert 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'erase 'class/hset/erase 'static '(r0 r1 r2) '(r0))
(dec-method 'copy 'class/hset/copy 'static '(r0 r1) '(r0 r1))
(dec-method 'get_iters 'class/hset/get_iters 'static '(r0) '(r0 r1 r2))
(dec-method 'key_callback 'class/obj/null 'static '(r0 r1) '(r0 r1))

(dec-method 'deinit 'class/hset/deinit 'override)

(def-struct 'hset 'obj)
	(uint 'num_buckets)
	(ptr 'buckets)
	(ptr 'key_callback)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/hset/num_buckets ()
	;inputs
	;r0 = hash_set object (ptr)
	;outputs
	;r0 = hash_set object (ptr)
	;r1 = num buckets (uint)
	(vp-cpy-ir-ui r0 hset_num_buckets r1))
