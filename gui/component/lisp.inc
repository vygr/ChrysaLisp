;lisp bindings

(defun-bind component-get-id (component)
	;(component-get-id component) -> id
	(get-field component (const component_id) (const long_size)))
