(import "class/obj/lisp.inc")
(import "./struct.inc")

(defun stdio-get-args (stdio)
	; (stdio-get-args stdio) -> cmd_line
	(getf stdio +stdio_args 0))