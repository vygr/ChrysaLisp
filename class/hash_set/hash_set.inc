(import 'class/obj/obj.inc)

(def-class 'hash_set 'obj)
(def-method 'create 'class/hash_set/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/hash_set/new 'static nil '(r0))
(def-method 'init 'class/hash_set/init 'static '(r0 r1 r2 r3))
(def-method 'num_buckets 'class/hash_set/num_buckets 'static '(r0) '(r0 r1))
(def-method 'get_bucket 'class/hash_set/get_bucket 'static '(r0 r1) '(r0 r1))
(def-method 'clear 'class/hash_set/clear 'static '(r0) '(r0))
(def-method 'for_each 'class/hash_set/for_each 'static '(r0 r1 r2) '(r0 r1 r2))
(def-method 'find 'class/hash_set/find 'static '(r0 r1) '(r0 r1 r2))
(def-method 'insert 'class/hash_set/insert 'static '(r0 r1) '(r0 r1 r2))
(def-method 'erase 'class/hash_set/erase 'static '(r0 r1 r2) '(r0))
(def-method 'copy 'class/hash_set/copy 'static '(r0 r1) '(r0 r1))
(def-method 'get_iters 'class/hash_set/get_iters 'static '(r0) '(r0 r1 r2))
(def-method 'key_callback 'class/obj/null 'static '(r0 r1) '(r0 r1))

(def-method 'deinit 'class/hash_set/deinit 'override)

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
	;r0 = hash_set object
	;outputs
	;r0 = hash_set object
	;r1 = num buckets
	(vp-cpy-ir-ui r0 hash_set_num_buckets r1))
