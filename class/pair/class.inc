(include 'class/obj/class.inc)

(def-class 'pair 'obj)
(dec-method 'vtable 'class/pair/vtable)
(dec-method 'create 'class/pair/create 'static '(r0 r1) '(r0))
(dec-method 'init 'class/pair/init 'static '(r0 r1 r2 r3))
(dec-method 'ref_first 'class/pair/ref_first 'static '(r0) '(r0 r1))
(dec-method 'ref_second 'class/pair/ref_second 'static '(r0) '(r0 r1))
(dec-method 'get_first 'class/pair/get_first 'static '(r0) '(r0 r1))
(dec-method 'get_second 'class/pair/get_second 'static '(r0) '(r0 r1))
(dec-method 'set_first 'class/pair/set_first 'static '(r0 r1) '(r0))
(dec-method 'set_second 'class/pair/set_second 'static '(r0 r1) '(r0))

(dec-method 'deinit 'class/pair/deinit 'final)

(def-struct 'pair 'obj)
	(ptr 'first)
	(ptr 'second)
(def-struct-end)

;;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;;

(defcfun class/pair/init ()
	;inputs
	;r0 = pair object (ptr)
	;r1 = vtable (pptr)
	;r2 = first object (ptr)
	;r3 = second object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = 0 if error, else ok
	;trashes
	;r1
	(assign '(r2 r3) '((r0 pair_first) (r0 pair_second)))
	(s-call 'pair 'init '(r0 r1) '(r0 r1)))

(defcfun-bind class/pair/get_first ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;trashes
	;r1
	(assign '((r0 pair_first)) '(r1)))

(defcfun-bind class/pair/get_second ()
	;inputs
	;r0 = pair object (ptr)
	;outputs
	;r0 = pair object (ptr)
	;r1 = object (ptr)
	;trashes
	;r1
	(assign '((r0 pair_second)) '(r1)))
