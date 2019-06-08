(include 'class/hash_set/class.inc)

(def-class 'hash_map 'hash_set)
(dec-method 'vtable 'class/hash_map/vtable)
(dec-method 'create 'class/hash_map/create)
(dec-method 'init 'class/hash_map/init)
(dec-method 'find 'class/hash_map/find)
(dec-method 'copy 'class/hash_map/copy)
(dec-method 'insert 'class/hash_map/insert 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'search 'class/hash_map/search 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'set 'class/hash_map/set 'static '(r0 r1 r2) '(r0 r1))
(dec-method 'get 'class/hash_map/get 'static '(r0 r1) '(r0 r1))
(dec-method 'get_parent 'class/hash_map/get_parent 'static '(r0) '(r0 r1))
(dec-method 'set_parent 'class/hash_map/set_parent 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/hash_map/deinit 'override)

(dec-method 'lisp_env 'class/hash_map/lisp_env 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_def 'class/hash_map/lisp_def 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_defq 'class/hash_map/lisp_defq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set 'class/hash_map/lisp_set 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_setq 'class/hash_map/lisp_setq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_defined 'class/hash_map/lisp_defined 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_undef 'class/hash_map/lisp_undef 'static '(r0 r1) '(r0 r1))

(def-struct 'hash_map 'hash_set)
	(ptr 'parent)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/hash_map/get_parent ()
	;inputs
	;r0 = hash_map object (ptr)
	;outputs
	;r0 = hash_map object (ptr)
	;r1 = 0, else hash_map parent object (ptr)
	(vp-cpy-ir r0 hash_map_parent r1))
