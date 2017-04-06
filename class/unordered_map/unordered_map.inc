(import 'class/unordered_set/unordered_set.inc)

(def-class 'unordered_map 'unordered_set)
(def-method 'create 'class/unordered_map/create)
(def-method 'new 'class/unordered_map/new)
(def-method 'init 'class/unordered_map/init)
(def-method 'find 'class/unordered_map/find)
(def-method 'copy 'class/unordered_map/copy)
(def-method 'insert 'class/unordered_map/insert 'static '(r0 r1 r2) '(r0 r1 r2))
(def-method 'get_parent 'class/unordered_map/get_parent 'static '(r0) '(r0 r1))
(def-method 'set_parent 'class/unordered_map/set_parent 'static '(r0 r1) '(r0))

(def-method 'deinit 'class/unordered_map/deinit 'final)

(def-struct 'unordered_map 'unordered_set)
	(ptr 'parent)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/unordered_map/get_parent ()
	;inputs
	;r0 = unordered_map object
	;outputs
	;r0 = unordered_map object
	;r1 = 0, else unordered_map parent
	(vp-cpy-ir r0 unordered_map_parent r1))
