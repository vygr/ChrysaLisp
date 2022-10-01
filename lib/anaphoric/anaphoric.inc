;;;;;;;;;;;;;;;;;;
; Anaphoric macros
;;;;;;;;;;;;;;;;;;

(defmacro aif (x y &optional z)
	; (aif form form [form])
	`(let ((it ,x))
		(if it ,y ,z)))

(defmacro awhen (x &rest _)
	; (awhen form body)
	`(let ((it ,x))
		(when it ~_)))

(defmacro awhile (x &rest _)
	; (awhile form body)
	`(while (defq ,(defq _x (gensym)) ,x)
		(let ((it ,_x)) ~_)))

(defmacro aand (&rest _)
	; (aand [form] ...)
	(cond
		((= (length _) 0))
		((= (length _) 1) (elem-get 0 _))
		(:t `(aif ,(elem-get 0 _) (aand ~(slice 1 -1 _))))))

(defmacro acond (&rest _)
	; (acond (tst body) ...)
	(reduce (lambda (x _)
		(push x `((defq ,(defq _x (gensym)) ,(elem-get 0 _)) (let ((it ,_x)) ~(slice 1 -1 _)))))
			_ (list 'cond)))

(defmacro aeach (x &rest _)
	; (aeach seq body)
	`(each (lambda (it) ~_) ,x))

(defmacro asome (x &rest _)
	; (asome seq body)
	`(some (lambda (it) ~_) ,x))
