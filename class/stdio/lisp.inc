;lisp bindings
(ffi create-stdio "class/stdio/lisp_create" 0)

(defun-bind stdio-get-args (stdio)
	;(stdio-get-args stdio) -> list
	(get-field stdio (const stdio_args) 0))
