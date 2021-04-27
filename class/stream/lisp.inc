(import "class/obj/lisp.inc")

(ffi stream-avail "class/stream/lisp_available" 0)
(ffi stream-flush "class/stream/lisp_write_flush" 0)
(ffi stream-seek "class/stream/lisp_seek" 0)

; Lisp (getf ...)

(structure +stream +obj_size
	(ptr object buffer)
	(pubyte bufp bufe))
