;lisp bindings

(defun-bind component-get-id (component)
	;(component-get-id component) -> id
	(get-field component component_id (const long_size)))
