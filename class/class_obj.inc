(import 'inc/class.inc)

(def-class 'obj)
(def-method 'create 'class/obj/create 'static '() '(r0))
(def-method 'destroy 'class/obj/destroy 'static '(r0))
(def-method 'new 'class/obj/new 'static '() '(r0))
(def-method 'init 'class/obj/init 'static '(r0 r1) '(r1))
(def-method 'inst_of 'class/obj/inst_of 'static '(r0 r1) '(r1))

(def-method 'delete 'sys/mem_free 'virtual '(r0))
(def-method 'deinit 'class/obj/null 'virtual '(r0))
(def-method 'hash 'class/obj/hash 'virtual '(r0) '(r1))

(def-struct 'obj)
	(pptr 'obj_vtable)
(def-struct-end)
