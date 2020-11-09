;lisp bindings

(ffi component-connect "gui/component/lisp_connect" 0)
(ffi component-emit "gui/component/lisp_emit" 0)

(defun component-get-id (component)
	; (component-get-id component) -> id
	(get-field component (const component_id) (const long_size)))
