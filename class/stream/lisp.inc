(import "class/obj/lisp.inc")

(ffi "class/stream/lisp_available" stream-avail)
; (stream-avail stream) -> num
(ffi "class/stream/lisp_write_flush" stream-flush)
; (stream-flush stream) -> stream
(ffi "class/stream/lisp_seek" stream-seek)
; (stream-seek stream offset pos) -> stream
(ffi "class/stream/lisp_read_bits" read-bits)
; (read-bits stream num_bits bit_pool bit_pool_size) -> (data|-1 bit_pool bit_pool_size)
(ffi "class/stream/lisp_write_bits" write-bits)
; (write-bits stream data num_bits bit_pool bit_pool_size) -> (bit_pool bit_pool_size)

; Lisp (getf ...)

(structure +stream +obj_size
	(ptr object buffer)
	(pubyte bufp bufe))
