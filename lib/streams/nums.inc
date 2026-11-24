;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; an array of numbers written to a stream, and read from one
;
; (write-nums stream nums) -> byte_count
; (read-nums stream byte_count nums) -> :nil | nums
;
; A nums, a fixeds, a path, a doubles or a floats, its elements as they
; are in memory, 8 bytes each and 4 of a floats. By (write-blk) and
; (read-blk), so the stream is any stream, a file's, a str's, one of a
; pipeline with a stage that packs it, lib/streams/stages.inc, and the
; writer and the reader know nothing of which.
;
; In Lisp for now, a str of the bytes made between each way: to be timed,
; and made the stream's own calls if that is worth it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-nums (stream array)
	; (write-nums stream nums) -> byte_count
	;the elements of an array of numbers, as bytes. Of none, nothing
	(defq n (* (length array) (if (floats? array) +int_size +long_size)))
	(if (> n 0) (write-blk stream (obj-set (str-alloc n) +str_data n array)) 0))

(defun read-nums (stream byte_count array)
	; (read-nums stream byte_count nums) -> :nil | nums
	;so many bytes read onto the end of an array of numbers, as its
	;elements, the array says of what kind. :nil at the stream's end
	(cond
		((<= byte_count 0) array)
		((defq blk (read-blk stream byte_count)) (push array blk))))
