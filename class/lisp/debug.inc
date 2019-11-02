;;;;;;;;;;
; Debuging
;;;;;;;;;;

(ffi kernel-debug "sys/kernel/lisp_debug" 0)
(defq *r* (list))

(defmacro-bind debug (_)
	;(debug form)
	`(progn
		(push *r* ,_)
		(kernel-debug (str (const (ascii-char 10)) (const (ascii-char 10))
			"Environment:" (const (ascii-char 10)) (env) (const (ascii-char 10))))
		(pop *r*)))

(defmacro-bind defun-debug (n a &rest _)
	;(defun-debug name ([arg ...]) body)
	`(defq ,n (lambda ,a ~(debug-fun _))))

(defun-bind is-debug-fun (_)
	(and (= (type-of _) lisp_type_lst) (/= 0 (length _))
		(not (or (eql 'quote (elem 0 _)) (eql 'debug (elem 0 _))))
		(= (type-of (elem 0 _)) lisp_type_sym) (setq _ (def? (elem 0 _)))
		(or (= (type-of _) lisp_type_fnc)
			(and (= (type-of _) lisp_type_lst) (/= 0 (length _)) (eql 'lambda (elem 0 _))))))

(defun-bind debug-fun (_)
	;(debug-fun list) -> list
	(defq _s (list (setq _ (copy _))))
	(while (defq _l (pop _s))
		(each! 0 -1 (lambda (_e)
			(when (is-debug-fun _e)
				(elem-set _ _l (cat '(debug) (list _e)))
				(push _s _e))) (list _l))) _)
