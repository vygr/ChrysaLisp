(import 'class/class_unordered_set.inc)

(def-class 'unordered_map 'unordered_set)
(def-method 'create 'class/unordered_map/create)
(def-method 'new 'class/unordered_map/new)
(def-method 'init 'class/unordered_map/init)
(def-method 'find 'class/unordered_map/find)
(def-method 'copy 'class/unordered_map/copy)
(def-method 'insert 'class/unordered_map/insert 'static '(r0 r1 r2) '(r1 r2))
(def-method 'get_parent 'class/unordered_map/get_parent 'static '(r0) '(r1))
(def-method 'set_parent 'class/unordered_map/set_parent 'static '(r0 r1))

(def-method 'deinit 'class/unordered_map/deinit 'override)
(def-method 'slice 'class/unordered_map/slice 'override)

(def-struct 'unordered_map 'unordered_set)
	(ptr 'unordered_map_parent)
(def-struct-end)
