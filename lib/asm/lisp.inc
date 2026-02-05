;;;;;;;;;;;;;;;;
; func structure
;;;;;;;;;;;;;;;;

(import "sys/list/struct.inc")

(structure +fn_header +ln_fnode_size
	(ushort length)
	(ushort entry)
	(ushort links)
	(ushort paths)
	(ushort stack)
	(offset pathname))
