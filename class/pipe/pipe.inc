(import 'class/obj/obj.inc)
(import 'class/vector/vector.inc)

(def-class 'pipe 'obj)
(def-method 'create 'class/pipe/create 'static '(r0 r1) '(r0))
(def-method 'new 'class/pipe/new 'static nil '(r0))
(def-method 'init 'class/pipe/init 'static '(r0 r1 r2 r3))
(def-method 'select 'class/pipe/select 'static '(r0 r1) '(r0 r1))
(def-method 'get_state 'class/pipe/get_state 'static '(r0) '(r0 r1))
(def-method 'set_state 'class/pipe/set_state 'static '(r0 r1) '(r0))
(def-method 'get_input 'class/pipe/get_input 'static '(r0) '(r0 r1))

(def-method 'deinit 'class/pipe/deinit 'final)

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
	;r1 = input stream object
	(vp-cpy-ir r0 pipe_streams r1)
	(vp-cpy-ir r1 array_array r1)
	(vp-cpy-ir r1 0 r1))

(defcfun class/pipe/get_state ()
	;inputs
	;r0 = pipe object (ptr)
	;outputs
	;r0 = pipe object (ptr)
	;r1 = current state
	(vp-cpy-ir r0 pipe_state r1))

(defcfun class/pipe/set_state ()
	;inputs
	;r0 = pipe object (ptr)
	;r1 = current state
	;outputs
	;r0 = pipe object (ptr)
	(vp-cpy-ri r1 r0 pipe_state))
