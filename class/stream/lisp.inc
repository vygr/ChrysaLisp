(import "class/obj/lisp.inc")

(ffi "class/stream/lisp_avail" stream-avail)
; (stream-avail stream) -> num
(ffi "class/stream/lisp_flush" stream-flush)
; (stream-flush stream) -> stream
(ffi "class/stream/lisp_seek" stream-seek)
; (stream-seek stream offset pos) -> stream
(ffi "class/stream/lisp_read_bits" read-bits)
; (read-bits stream (array bit_pool bit_pool_size) num_bits) -> (data|-1)
(ffi "class/stream/lisp_write_bits" write-bits)
; (write-bits stream (array bit_pool bit_pool_size) data num_bits) -> stream

(defun flush-bits (stream bit_pool_state)
	; (flush-bits stream (array bit_pool bit_pool_size))
    (when (> (defq current_size (elem-get bit_pool_state 1)) 0)
        (when (> (defq remainder (% current_size 8)) 0)
            (write-bits stream bit_pool_state 0 (- 8 remainder)))))

; Lisp (getf ...)

(structure +stream +obj_size
	(ptr object buffer)
	(pubyte bufp bufe))
