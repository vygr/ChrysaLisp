;;;;;;;;;;
; Debuging
;;;;;;;;;;

(import "apps/debug/app.inc")

(defq *r* (list))

(defun debug-format (n e)
	; (debug-format name env)
	(setq e (tolist e))
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
			(setq val (apply cat (map (# (if (<= 32 (code %0) 126) %0 ".")) val))))
		(write msg (cat var " : " val (ascii-char 10)))) e)
	(debug-send (str msg (ascii-char 10))))

(defmacro debug (n _)
	; (debug name form)
	`(progn
		(push *r* ,_)
		(debug-format ,n (env))
		(pop *r*)))

(defun debug-fun? (_)
	; (debug-fun? form)
	(and (list? _) (/= 0 (length _))
		(not (or (eql 'quote (elem 0 _)) (eql 'debug (elem 0 _))))
		(sym? (elem 0 _)) (setq _ (get (elem 0 _)))
		(or (func? _) (lambda? _))))

(defun debug-fun (n _)
	; (debug-fun name list) -> list
	(defq s (list _))
	(while (defq l (pop s))
		(each (lambda (e)
			(when (debug-fun? e)
				(elem-set _ l (cat '(debug) (list n e)))
				(push s e))) l)) _)

(defmacro defun (n a &rest _)
	`(defq ,n (lambda ,a ~(macroexpand (debug-fun (str n) _)))))
