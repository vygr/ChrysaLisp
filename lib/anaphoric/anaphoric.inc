;;;;;;;;;;;;;;;;;;
; Anaphoric macros
;;;;;;;;;;;;;;;;;;

(defmacro-bind aif (x y &optional z)
	;(aif form form [form])
	`(let ((it ,x))
		(if it ,y ,z)))

(defmacro-bind awhen (x &rest _)
	;(awhen form body)
	`(let ((it ,x))
		(when it ~_)))

(defmacro-bind awhile (x &rest _)
	;(awhile form body)
	`(while (defq ,(defq _x (gensym)) ,x)
		(let ((it ,_x)) ~_)))

(defmacro-bind aand (&rest _)
	;(aand [form] ...)
	(cond
		((= (length _) 0))
		((= (length _) 1) (elem 0 _))
		(t `(aif ,(elem 0 _) (aand ~(slice 1 -1 _))))))

(defmacro-bind acond (&rest _)
	;(acond (tst body) ...)
	(reduce (lambda (x _)
		(push x `((defq ,(defq _x (gensym)) ,(elem 0 _)) (let ((it ,_x)) ~(slice 1 -1 _)))))
			_ (list 'cond)))
