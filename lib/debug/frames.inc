;;;;;;;;;;;;;;
; Stack frames
;;;;;;;;;;;;;;

;module
(env-push)

(setq *stack_frame* (list))

(defun stack-instrument (name form)
	`((push *stack_frame* ,(str name " -> " *stream_name* "(" *stream_line* ")"))
	(defq __r__ (progn ~form))
	(pop *stack_frame*) __r__))

(defmacro defun (n a &rest _)
	`(defq ,n (lambda ,a ~(stack-instrument n _))))

(defmacro defmethod (n a &rest _)
	`(def (def? :vtable this) ,n (lambda (this ~a) ~(stack-instrument (cat *class* " " n) _))))

;module
(export-symbols '(defun defmethod stack-instrument))
(env-pop)
