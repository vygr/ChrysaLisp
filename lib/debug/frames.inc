;;;;;;;;;;;;;;
; Stack frames
;;;;;;;;;;;;;;

(defq *stack_frame* (list) *stack_rvals* (list))

;module
(env-push)

(defun stack-frame (name form)
	(static-qq
		((push *stack_frame* ,(str n " -> " *stream_name* "(" *stream_line* ")"))
		(push *stack_rvals* (progn ~form))
		(pop *stack_frame*)
		(pop *stack_rvals*))))

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(stack-frame n _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(stack_frame (cat *class* " " n) _))))))

;module
(export-symbols defun defmethod)
(env-pop)
