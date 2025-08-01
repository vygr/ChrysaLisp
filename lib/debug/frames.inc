;;;;;;;;;;;;;;
; Stack frames
;;;;;;;;;;;;;;

;module
(env-push)

(setq *stack_frame* (list))

(defun stack-instrument (name form)
	(bind '(name line) (repl-info))
	`((push *stack_frame* ,(str name " -> " name "(" line ")"))
	(defq __r__ (progn ~form))
	(pop *stack_frame*) __r__))

(redefmacro defun (n a &rest _)
	(static-qq (defq ,n (,'lambda ,a ~(stack-instrument n _)))))

(redefmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (,'lambda (this ~a)
		~(stack-instrument (cat *class* " " n) _)))))

;module
(export-symbols '(defun defmethod))
(env-pop)
