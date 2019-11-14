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

(dec-method 'lisp_get_field 'class/obj/lisp_get_field 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_set_field 'class/obj/lisp_set_field 'static '(r0 r1) '(r0 r1))

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
	(assign '(r1 1) '((r0 obj_vtable) r1))
	(assign '(r1) '((r0 obj_count))))

(defcfun class/obj/ref (&optional o c x)
	;inputs
	;r0 = object (ptr)
	;outputs
	;r0 = object (ptr)
	;trashes
	;r1
	(setd o r0 c r1 x 1)
	(if (eql o c) (throw "Invalid object or count !" (list o c)))
	(assign `((,o obj_count)) `(,c))
	(vp-add-cr x c)
	(assign `(,c) `((,o obj_count))))
