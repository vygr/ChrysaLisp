(include 'class/obj/class.inc)
(include 'class/vector/class.inc)

(def-class 'pipe 'obj)
(dec-method 'vtable 'class/pipe/vtable)
(dec-method 'create 'class/pipe/create 'static '(r0 r1) '(r0))
(dec-method 'init 'class/pipe/init 'static '(r0 r1 r2 r3))
(dec-method 'select 'class/pipe/select 'static '(r0 r1) '(r0 r1))
(dec-method 'get_state 'class/pipe/get_state 'static '(r0) '(r0 r1))
(dec-method 'set_state 'class/pipe/set_state 'static '(r0 r1) '(r0 r1))
(dec-method 'get_input 'class/pipe/get_input 'static '(r0) '(r0 r1))

(dec-method 'deinit 'class/pipe/deinit 'final)

(dec-method 'lisp_create 'class/pipe/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_read 'class/pipe/lisp_read 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_write 'class/pipe/lisp_write 'static '(r0 r1) '(r0 r1))

(def-struct 'pipe 'obj)
	(local-align)
	(offset 'start)
	(ptr 'streams)
	(pptr 'select_array)
	(ulong 'state)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/pipe/get_input ()
	;inputs
	;r0 = pipe object (ptr)
	;outputs
	;r0 = pipe object (ptr)
	;r1 = input stream object (ptr)
	;trashes
	;r1
	(vp-cpy-ir r0 pipe_streams r1)
	(vp-cpy-ir r1 array_array r1)
	(vp-cpy-ir r1 0 r1))

(defcfun class/pipe/get_state ()
	;inputs
	;r0 = pipe object (ptr)
	;outputs
	;r0 = pipe object (ptr)
	;r1 = current state (ulong)
	;trashes
	;r1
	(vp-cpy-ir r0 pipe_state r1))

(defcfun class/pipe/set_state ()
	;inputs
	;r0 = pipe object (ptr)
	;r1 = current state (ulong)
	;outputs
	;r0 = pipe object (ptr)
	;r1 = current state (ulong)
	;trashes
	;none
	(vp-cpy-ri r1 r0 pipe_state))
