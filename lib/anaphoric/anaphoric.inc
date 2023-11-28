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
		((= (length _) 1) (first _))
		(:t `(aif ,(first _) (aand ~(rest _))))))

(defmacro acond (&rest _)
	; (acond (tst body) ...)
	(reduce (lambda (x _)
		(push x `((defq ,(defq _x (gensym)) ,(first _)) (let ((it ,_x)) ~(rest _)))))
			_ (list 'cond)))

(defmacro aeach (x &rest _)
	; (aeach seq body)
	`(each (lambda (it) ~_) ,x))

(defmacro asome (x &rest _)
	; (asome seq body)
	`(some (lambda (it) ~_) ,x))
