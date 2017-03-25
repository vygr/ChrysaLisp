(import 'class/ref/ref.inc)
(import 'class/vector/vector.inc)

(def-class 'master 'ref)
(def-method 'create 'class/master/create 'static nil '(r0))
(def-method 'new 'class/master/new 'static nil '(r0))
(def-method 'init 'class/master/init 'static '(r0 r1) '(r1))
(def-method 'start 'class/master/start 'static '(r0 r1 r2))
(def-method 'stop 'class/master/stop 'static '(r0))
(def-method 'get_state 'class/master/get_state 'static '(r0) '(r1))
(def-method 'get_stream 'class/master/get_stream 'static '(r0 r1) '(r1))
(def-method 'select 'class/master/select 'static '(r0 r1) '(r1))
(def-method 'get_input 'class/master/get_input 'static '(r0) '(r1))

(def-method 'deinit 'class/master/deinit 'final)

(def-struct 'master 'ref)
	(local-align)
	(offset 'master_start)
	(ptr 'master_streams)
	(ptr 'master_mailbox_array)
	(pptr 'master_select_array)
	(ulong 'master_state)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/master/get_input ()
	;inputs
	;r0 = master object
	;outputs
	;r0 = master object
	;r1 = input stream object
	(vp-cpy-ir r0 master_streams r1)
	(vp-cpy-ir r1 vector_array r1)
	(vp-cpy-ir r1 0 r1))

(defcfun class/master/get_state ()
	;inputs
	;r0 = master object
	;outputs
	;r0 = master object
	;r1 = current state
	(vp-cpy-ir r0 master_state r1))
