;;;;;;;;;;
; Debuging
;;;;;;;;;;

(defq *r* (list))

(defun-bind debug-send (s)
	;(debug-send string)
	(when (/= 0 (length (defq services (mail-enquire "DEBUG_SERVICE"))))
		(defq mbox (mail-alloc-mbox) debug_mbox (str-to-num (elem 1 (split (pop services) ","))))
		(mail-send (cat (char 0 long_size) (char mbox long_size) (char (task-mailbox) long_size) s) debug_mbox)
		(mail-read mbox)
		(mail-free-mbox mbox)))

(defun-bind debug-str (_)
	;(debug-str str) -> str
	(apply cat (map (# (if (or (< (code %0) 32) (= (code %0) 34)) "." %0)) _)))

(defun-bind debug-format (n e)
	;(debug-format name env)
	(setq e (tolist e))
	(each (# (if (str? (elem 1 %0)) (elem-set 1 %0 (debug-str %0)))) e)
	(sort (# (cmp (elem 0 %0) (elem 0 %1))) e)
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
	(debug-send (str msg (ascii-char 10))))

(defmacro-bind debug (n _)
	;(debug name form)
	`(progn
		(push *r* ,_)
		(debug-format ,n (env))
		(pop *r*)))

(defun-bind debug-fun? (_)
	;(debug-fun? form)
	(and (= (type-of _) lisp_type_lst) (/= 0 (length _))
		(not (or (eql 'quote (elem 0 _)) (eql 'debug (elem 0 _))))
		(= (type-of (elem 0 _)) lisp_type_sym) (setq _ (get (elem 0 _)))
		(or (= (type-of _) lisp_type_fnc)
			(and (= (type-of _) lisp_type_lst) (/= 0 (length _)) (eql 'lambda (elem 0 _))))))

(defun-bind debug-fun (n _)
	;(debug-fun name list) -> list
	(defq s (list _))
	(while (defq l (pop s))
		(each (lambda (e)
			(when (debug-fun? e)
				(elem-set _ l (cat '(debug) (list n e)))
				(push s e))) l)) _)

(defmacro-bind defun-debug (n a &rest _)
	;(defun-debug name ([arg ...]) body)
	`(defq ,n (lambda ,a ~(prebind (macroexpand (debug-fun (str n) _))))))
