(import "class/obj/lisp.inc")

(ffi create-stdio "class/stdio/lisp_create" 0)

(structure +stdio +obj_size
	(ptr stdin stdout stderr args))

(defun stdio-get-args (stdio)
	; (stdio-get-args stdio) -> list
	(getf stdio +stdio_args 0))
