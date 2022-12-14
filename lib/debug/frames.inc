;;;;;;;;;;;;;;
; Stack frames
;;;;;;;;;;;;;;

(when (> *debug_mode* 0)
	(import "sys/lisp.inc")

	(defq *stack_frame* (list) *stack_return_vals* (list))

	(defmacro framed-lambda (n a &rest _)
		`(lambda ,a
			(push *stack_frame* ,(str n " -> " *stream_name* "(" *stream_line* ")"))
			(push *stack_return_vals* (progn ~_))
			(pop *stack_frame*)
			(pop *stack_return_vals*)))

	(defmacro defun (n a &rest _)
		`(defq ,n (framed-lambda ,n ,a ~(macroexpand _))))

	(defmacro defmethod (n a &rest _)
		`(def (def? :vtable this) ,n (framed-lambda ,(sym (cat *class* " " n)) ,a ~(macroexpand _))))
)
