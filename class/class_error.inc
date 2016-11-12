(import 'class/class_ref.inc)

(def-class 'error 'ref)
(def-method 'create 'class/error/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/error/new)
(def-method 'init 'class/error/init 'static '(r0 r1 r2 r3) '(r1))
(def-method 'get_description 'class/error/get_description 'static '(r0) '(r1))
(def-method 'get_object 'class/error/get_object 'static '(r0) '(r1))

(def-method 'deinit 'class/error/deinit 'override)

(def-struct 'error 'ref)
	(ptr 'error_description)
	(ptr 'error_object)
(def-struct-end)
