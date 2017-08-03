(import 'sys/class.inc)

(def-class 'obj)
(def-method 'destroy 'class/obj/destroy 'static '(r0))
(def-method 'init 'class/obj/init 'static '(r0 r1) '(r0 r1))
(def-method 'inst_of 'class/obj/inst_of 'static '(r0 r1) '(r0 r1))

(def-method 'delete 'sys/mem_free 'final '(r0))
(def-method 'deinit 'class/obj/null 'virtual '(r0))
(def-method 'hash 'class/obj/hash 'virtual '(r0) '(r0 r1))

(def-struct 'obj)
	(pptr 'vtable)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/obj/init ()
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r1 = 0 if error, else ok
	(vp-cpy-ri r1 r0 obj_vtable))
