(import "class/obj/lisp.inc")

(ffi "class/stream/lisp_available" stream-avail)
; (stream-avail stream) -> num
(ffi "class/stream/lisp_write_flush" stream-flush)
; (stream-flush stream) -> stream
(ffi "class/stream/lisp_seek" stream-seek)
; (stream-seek stream offset pos) -> stream

; Lisp (getf ...)

(structure +stream +obj_size
	(ptr object buffer)
	(pubyte bufp bufe))
