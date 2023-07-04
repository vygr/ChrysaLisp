;;;;;;;;;;;;;;;;
; func structure
;;;;;;;;;;;;;;;;

; Lisp (getf ...)

(import "sys/list/lisp.inc")

(structure +fn_header +ln_fnode_size
	(ushort length)
	(ushort entry)
	(ushort links)
	(ushort paths)
	(ushort stack)
	(offset pathname))
