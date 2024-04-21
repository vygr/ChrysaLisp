(import "class/obj/lisp.inc")

(ffi stream-avail "class/stream/lisp_available" 0)
; (stream-avail stream) -> num
(ffi stream-flush "class/stream/lisp_write_flush" 0)
; (stream-flush stream) -> stream
(ffi stream-seek "class/stream/lisp_seek" 0)
; (stream-seek stream offset pos) -> stream

; Lisp (getf ...)

(structure +stream +obj_size
	(ptr object buffer)
	(pubyte bufp bufe))
