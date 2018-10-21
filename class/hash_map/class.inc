(include 'class/hash_set/class.inc)

(def-class 'hash_map 'hash_set)
(dec-method 'vtable 'class/class_hash_map)
(dec-method 'create 'class/hash_map/create)
(dec-method 'new 'class/hash_map/new)
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
