(import 'class/obj/obj.inc)

(def-class 'ref 'obj)
(def-method 'init 'class/ref/init)
(def-method 'ref 'class/ref/ref 'static '(r0) '(r0))
(def-method 'deref 'class/ref/deref 'static '(r0) '(r0))
(def-method 'ref_if 'class/ref/ref_if 'static '(r0) '(r0))
(def-method 'deref_if 'class/ref/deref_if 'static '(r0) '(r0))

(def-struct 'ref 'obj)
	(uint 'count)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/ref/init ()
	;inputs
	;r0 = object
	;r1 = vtable pointer
	;outputs
	;r0 = object
	;r1 = 0 if error, else ok
	(s-call 'ref 'init '(r0 r1))
	(vp-cpy-cr 1 r1)
	(vp-cpy-ri-i r1 r0 ref_count))

(defcfun class/ref/ref ()
	;inputs
	;r0 = object
	;trashes
	;r1
	(vp-cpy-ir-ui r0 ref_count r1)
	(vp-add-cr 1 r1)
	(vp-cpy-ri-i r1 r0 ref_count))
