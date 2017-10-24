(import 'class/ref/ref.inc)
(import 'class/vector/vector.inc)

(def-class 'pipe 'ref)
(def-method 'create 'class/pipe/create 'static nil '(r0))
(def-method 'new 'class/pipe/new 'static nil '(r0))
(def-method 'init 'class/pipe/init 'static '(r0 r1))
(def-method 'start 'class/pipe/start 'static '(r0 r1 r2) '(r0))
(def-method 'stop 'class/pipe/stop 'static '(r0) '(r0))
(def-method 'get_state 'class/pipe/get_state 'static '(r0) '(r0 r1))
(def-method 'select 'class/pipe/select 'static '(r0 r1) '(r0 r1))
(def-method 'get_input 'class/pipe/get_input 'static '(r0) '(r0 r1))

(def-method 'deinit 'class/pipe/deinit 'final)

(def-struct 'pipe 'ref)
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
	;r0 = pipe object
	;outputs
	;r0 = pipe object
	;r1 = input stream object
	(vp-cpy-ir r0 pipe_streams r1)
	(vp-cpy-ir r1 array_array r1)
	(vp-cpy-ir r1 0 r1))

(defcfun class/pipe/get_state ()
	;inputs
	;r0 = pipe object
	;outputs
	;r0 = pipe object
	;r1 = current state
	(vp-cpy-ir r0 pipe_state r1))
