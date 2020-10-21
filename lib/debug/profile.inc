;;;;;;;;;;;
; Profiling
;;;;;;;;;;;

(defq *profile* (env -401)
	profile-dump (lambda ()
		(each print (sort (# (- (elem 1 %1) (elem 1 %0))) (tolist *profile*)))
		(print)))

(defmacro defun (n a &rest _)
	`(defq ,n (lambda ,a
		(if (defq _c (get ',n *profile*))
			(set *profile* ',n (inc _c))
			(def *profile* ',n 0))
		~_)))

(defmacro defun-bind (n a &rest _)
	`(defq ,n (lambda ,a
		(if (defq _c (get ',n *profile*))
			(set *profile* ',n (inc _c))
			(def *profile* ',n 0))
		~(prebind (macroexpand _)))))
