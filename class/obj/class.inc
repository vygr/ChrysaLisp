(include 'sys/class.inc)

(def-class 'obj)
(dec-method 'vtable 'class/obj/vtable)
(dec-method 'null 'class/obj/null)
(dec-method 'destroy 'class/obj/destroy 'static '(r0))
(dec-method 'init 'class/obj/init 'static '(r0 r1) '(r0 r1))
(dec-method 'inst_of 'class/obj/inst_of 'static '(r0 r1) '(r0 r1))
(dec-method 'ref 'class/obj/ref 'static '(r0) '(r0))
(dec-method 'deref 'class/obj/deref 'static '(r0))
(dec-method 'ref_if 'class/obj/ref_if 'static '(r0) '(r0))
(dec-method 'deref_if 'class/obj/deref_if 'static '(r0))

(dec-method 'deinit 'class/obj/null 'virtual '(r0) '(r0))
(dec-method 'hash 'class/obj/hash 'virtual '(r0) '(r0 r1))

(def-struct 'obj)
	(pptr 'vtable)
	(uint 'count)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun class/obj/init ()
	;inputs
	;r0 = object (ptr)
	;r1 = vtable (pptr)
	;outputs
	;r0 = object (ptr)
	;r1 = 0 if error, else ok
	;trashes
	;r1
	(vp-cpy-ri r1 r0 obj_vtable)
	(vp-cpy-cr 1 r1)
	(vp-cpy-ri-i r1 r0 obj_count))

(defcfun class/obj/ref ()
	;inputs
	;r0 = object (ptr)
	;outputs
	;r0 = object (ptr)
	;trashes
	;r1
	(vp-cpy-ir-ui r0 obj_count r1)
	(vp-add-cr 1 r1)
	(vp-cpy-ri-i r1 r0 obj_count))
