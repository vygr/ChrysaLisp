(import 'class/ref/ref.inc)

(def-class 'slave 'ref)
(def-method 'create 'class/slave/create 'static nil '(r0))
(def-method 'new 'class/slave/new 'static nil '(r0))
(def-method 'init 'class/slave/init 'static '(r0 r1))
(def-method 'get_args 'class/slave/get_args 'static '(r0) '(r0 r1))

(def-method 'deinit 'class/slave/deinit 'final)

(def-method 'lisp_create 'class/slave/lisp_create 'static '(r0 r1) '(r0 r1))

(def-struct 'slave 'ref)
	(ptr 'stdin)
	(ptr 'stdout)
	(ptr 'stderr)
	(ptr 'args)
(def-struct-end)

(def-struct 'slave_msg_init)
	(ptr 'stdin_mbox)
	(struct 'stdout_id 'id)
	(struct 'stderr_id 'id)
	(struct 'ack_id 'id)
	(offset 'args)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;

(defcfun class/slave/get_args ()
	;inputs
	;r0 = slave object
	;outputs
	;r0 = slave object
	;r1 = command args
	(vp-cpy-ir r0 slave_args r1))
