(import 'class/obj/obj.inc)

(def-class 'pair 'obj)
(def-method 'vtable 'class/class_pair)
(def-method 'create 'class/pair/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/pair/new 'static nil '(r0))
(def-method 'init 'class/pair/init 'static '(r0 r1 r2 r3))
(def-method 'ref_first 'class/pair/ref_first 'static '(r0) '(r0 r1))
(def-method 'ref_second 'class/pair/ref_second 'static '(r0) '(r0 r1))
(def-method 'get_first 'class/pair/get_first 'static '(r0) '(r0 r1))
(def-method 'get_second 'class/pair/get_second 'static '(r0) '(r0 r1))
(def-method 'set_first 'class/pair/set_first 'static '(r0 r1) '(r0))
(def-method 'set_second 'class/pair/set_second 'static '(r0 r1) '(r0))

(def-method 'deinit 'class/pair/deinit 'final)

(def-struct 'pair 'obj)
	(ptr 'first)
	(ptr 'second)
(def-struct-end)

;;;;;;;;;;;;;;;;
;;inline methods
;;;;;;;;;;;;;;;;

(defcfun class/pair/init ()
	;inputs
	;r0 = pair object (ptr)
	;r1 = vtable (pptr)
	;r2 = first object (ptr)
	;r3 = second object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = 0 if error, else ok
	(vp-cpy-ri r2 r0 pair_first)
	(vp-cpy-ri r3 r0 pair_second)
	(s-call 'pair 'init '(r0 r1) '(r0 r1)))

(defcfun class/pair/get_first ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	(vp-cpy-ir r0 pair_first r1))

(defcfun class/pair/get_second ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	(vp-cpy-ir r0 pair_second r1))
