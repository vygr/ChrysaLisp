(include 'class/hset/class.inc)

(def-class 'hmap 'hset)
(dec-method 'vtable 'class/hmap/vtable)
(dec-method 'create 'class/hmap/create)
(dec-method 'init 'class/hmap/init)
(dec-method 'find 'class/hmap/find)
(dec-method 'copy 'class/hmap/copy)
(dec-method 'insert 'class/hmap/insert 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'search 'class/hmap/search 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'set 'class/hmap/set 'static '(r0 r1 r2) '(r0 r1))
(dec-method 'get 'class/hmap/get 'static '(r0 r1) '(r0 r1))
(dec-method 'get_parent 'class/hmap/get_parent 'static '(r0) '(r0 r1))
(dec-method 'set_parent 'class/hmap/set_parent 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/hmap/deinit 'override)

(dec-method 'lisp_env 'class/hmap/lisp_env 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_def 'class/hmap/lisp_def 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_defq 'class/hmap/lisp_defq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set 'class/hmap/lisp_set 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_setq 'class/hmap/lisp_setq 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_defined 'class/hmap/lisp_defined 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_undef 'class/hmap/lisp_undef 'static '(r0 r1) '(r0 r1))

(def-struct 'hmap 'hset)
	(ptr 'parent)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/hmap/get_parent ()
	;inputs
	;r0 = hash_map object (ptr)
	;outputs
	;r0 = hash_map object (ptr)
	;r1 = 0, else hash_map parent object (ptr)
	(vp-cpy-ir r0 hmap_parent r1))
