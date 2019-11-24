;lisp bindings
(ffi create-slave "class/slave/lisp_create" 0)

(defun-bind slave-get-args (slave)
	;(slave-get-args slave) -> list
	(get-field slave slave_args 0))
