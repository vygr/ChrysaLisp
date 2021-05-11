(import "class/obj/lisp.inc")

(ffi create-stdio "class/stdio/lisp_create" 0)

(structure +stdio +obj_size
	(ptr stdin stdout stderr args))

(structure +stdio_msg_init 0
	(netid stdout)
	(offset reply)
	(netid stderr ack)
	(offset args))

(defun stdio-get-args (stdio)
	; (stdio-get-args stdio) -> list
	(getf stdio +stdio_args 0))
