(import "class/obj/lisp.inc")
(import "./struct.inc")

(defun flush-bits (stream bit_pool_state)
	; (flush-bits stream (array bit_pool bit_pool_size))
	(when (> (defq current_size (elem-get bit_pool_state 1)) 0)
		(when (> (defq remainder (% current_size 8)) 0)
			(write-bits stream bit_pool_state 0 (- 8 remainder)))))
