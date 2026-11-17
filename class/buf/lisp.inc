;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; buffers, blocks of memory the nodes of this machine share
;
; A buffer is the block and nothing more, what is kept in it and how is
; for those who use it. (buf size) makes one, zero, under a key at random,
; and gives the key to the tasks that are to share it, in their mail; on
; their nodes (buf size key) opens the same block. The one that made it
; lets go of the key when it goes, so no more can open it, and the memory
; lasts till the last that has it lets go. Only on this machine: a task on
; another, given the key, has :nil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./struct.inc")

(ffi "class/buf/lisp_create" buf)
; (buf size [key]) -> :nil | buffer
(ffi "class/buf/lisp_get" buf-get)
; (buf-get buf offset type) -> val
(ffi "class/buf/lisp_set" buf-set)
; (buf-set buf offset type val) -> buf

(defun buf? (%0)
	; (buf? form) -> :t | :nil
	(find :buf (type-of %0)))

(defun buf-key (%0)
	; (buf-key buffer) -> key
	(getf %0 +buf_key 0))

(defun buf-size (%0)
	; (buf-size buffer) -> bytes
	(getf %0 +buf_bytes 0))
