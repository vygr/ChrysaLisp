(include 'class/obj/class.inc)

(def-class 'slave 'obj)
(dec-method 'vtable 'class/slave/vtable)
(dec-method 'create 'class/slave/create 'static nil '(r0))
(dec-method 'new 'class/slave/new 'static nil '(r0))
(dec-method 'init 'class/slave/init 'static '(r0 r1))
(dec-method 'get_args 'class/slave/get_args 'static '(r0) '(r0 r1))

(dec-method 'deinit 'class/slave/deinit 'final)

(dec-method 'lisp_create 'class/slave/lisp_create 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_get_args 'class/slave/lisp_get_args 'static '(r0 r1) '(r0 r1))

(def-struct 'slave 'obj)
	(ptr 'stdin)
	(ptr 'stdout)
	(ptr 'stderr)
	(ptr 'args)
(def-struct-end)

(def-struct 'slave_msg_init)
	(struct 'stdout_id 'id)
	(struct 'stderr_id 'id)
	(struct 'ack_id 'id)
	(uint 'stdin_mbox_id)
	(offset 'args)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/slave/get_args ()
	;inputs
	;r0 = slave object (ptr)
	;outputs
	;r0 = slave object (ptr)
	;r1 = command args
	(vp-cpy-ir r0 slave_args r1))
