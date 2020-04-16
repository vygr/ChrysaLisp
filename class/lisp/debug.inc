;;;;;;;;;;
; Debuging
;;;;;;;;;;

(ffi kernel-debug "sys/kernel/lisp_debug" 0)
(defq *r* (list))

(defun-bind debug-format (n e)
	;(debug-format name env)
	(bind '(e _) (read (string-stream (str e)) (ascii-code " ")))
	(sort (lambda (x y) (cmp (elem 0 x) (elem 0 y))) e)
	(write (defq msg (string-stream (cat "")))
		(cat (defq u (cat "+" (pad "" (+ (length n) 2) "--------") "+"))
			(const (cat (ascii-char 10) "| ")) (str n) (const (cat " |" (ascii-char 10)))
			u (ascii-char 10) (ascii-char 10)))
	(each (lambda ((var val))
		(setq var (pad (str var) 16) val (str val))
		(if (> (length val) 36)
			(setq val (cat (slice 0 36 val) " ...")))
		(if (> (length val) 0)
			(setq val (apply cat (map (lambda (_)
				(if (<= 32 (code _) 126) _ ".")) val))))
		(write msg (cat var " : " val (ascii-char 10)))) e)
	(kernel-debug (str msg (ascii-char 10))))

(defmacro-bind debug (n _)
	;(debug name form)
	`(progn
		(push *r* ,_)
		(debug-format ,n (env))
		(pop *r*)))

(defun-bind is-debug-fun (_)
	;(is-debug-fun form)
	(and (= (type-of _) lisp_type_lst) (/= 0 (length _))
		(not (or (eql 'quote (elem 0 _)) (eql 'debug (elem 0 _))))
		(= (type-of (elem 0 _)) lisp_type_sym) (setq _ (def? (elem 0 _)))
		(or (= (type-of _) lisp_type_fnc)
			(and (= (type-of _) lisp_type_lst) (/= 0 (length _)) (eql 'lambda (elem 0 _))))))

(defun-bind debug-fun (_n _)
	;(debug-fun name list) -> list
	(defq _s (list (setq _ (copy _))))
	(while (defq _l (pop _s))
		(each (lambda (_e)
			(when (is-debug-fun _e)
				(elem-set _ _l (cat '(debug) (list _n _e)))
				(push _s _e))) _l)) _)

(defmacro-bind defun-debug (n a &rest _)
	;(defun-debug name ([arg ...]) body)
	`(defq ,n (lambda ,a ~(debug-fun (str n) _))))
