;;;;;;;;;;
; Debuging
;;;;;;;;;;

(ffi kernel-debug "sys/kernel/lisp_debug" 0)
(defq *r* (list))

(defun-bind debug-format (_)
	;(debug-format env)
	(bind '(e _) (read (string-stream (str _)) (const (ascii-code " "))))
	(sort (lambda (x y) (cmp (elem 0 x) (elem 0 y))) e)
	(defq msg (string-stream (cat "")))
	(write msg (const (cat "Environment" (ascii-char 10) "-----------" (ascii-char 10))))
	(each (lambda ((var val))
		(setq var (pad (str var) 16) val (str val))
		(if (> (length val) 36) (setq val (cat (slice 0 36 val) " ...")))
		(if (> (length val) 0) (setq val (apply cat (map (lambda (_)
			(if (<= 32 (code _) 126) _ ".")) val))))
		(write msg (cat var " : " val (const (ascii-char 10))))) e)
	(kernel-debug (str msg (ascii-char 10))))

(defmacro-bind debug (_)
	;(debug form)
	`(progn
		(push *r* ,_)
		(debug-format (env))
		(pop *r*)))

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

(defmacro-bind defun-debug (n a &rest _)
	;(defun-debug name ([arg ...]) body)
	`(defq ,n (lambda ,a ~(debug-fun _))))
