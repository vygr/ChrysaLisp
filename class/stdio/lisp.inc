(import "class/obj/lisp.inc")

(ffi "class/stdio/lisp_create" create-stdio)
; (create-stdio) -> stdio

(structure +stdio +obj_size
	(ptr stdin stdout stderr args))

(structure +stdio_msg_init 0
	(netid stdout)
	(offset reply)
	(netid stderr ack)
	(offset args))

(defun stdio-get-args (stdio)
	; (stdio-get-args stdio) -> cmd_line
	(getf stdio +stdio_args 0))
