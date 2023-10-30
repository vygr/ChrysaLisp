;;;;;;;;;;;;;;
; Stack frames
;;;;;;;;;;;;;;

(defq *stack_frame* (list) *stack_return_vals* (list))

;module
(env-push)

(defun stack-frame (name form)
	; (stack-frame name form) -> form
	(static-qq (progn
		(push *stack_frame* ,(str n " -> " *stream_name* "(" *stream_line* ")"))
		(push *stack_return_vals* (progn ~_))
		(pop *stack_frame*)
		(pop *stack_return_vals*))))

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(stack-frame (str n) _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(stack_frame (cat *class* " " n) _))))))

;module
(export-symbols defun defmethod)
(env-pop)
