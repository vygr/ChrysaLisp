;;;;;;;;;;
; Debuging
;;;;;;;;;;

(when (> *debug_mode* 0)
	(import "apps/debug/app.inc")

	(defq *debug_return_vals* (list))

	(defun debug-trunc (s)
		(if (> (length s) 36)
			(cat (slice 0 36 s) " ...") s))

	(defun debug-format (n sform e)
		; (debug-format name sform env)
		(setq e (tolist e))
		(sort (# (cmp (first %0) (first %1))) e)
		(write (defq msg (string-stream (cat "")))
			(cat (defq u (cat "+" (pad "" (+ (length n) 2) "--------") "+"))
				(const (cat (ascii-char 10) "| ")) (str n) (const (cat " |" (ascii-char 10)))
				u (ascii-char 10) (ascii-char 10)
				(debug-trunc sform) (ascii-char 10) (ascii-char 10)))
		(each (lambda ((var val))
			(setq var (pad (str var) 16) val (debug-trunc (str val)))
			(if (> (length val) 0)
				(setq val (apply (const cat) (map (# (if (<= 32 (code %0) 126) %0 ".")) val))))
			(write msg (cat var " : " val (ascii-char 10)))) e)
		(debug-send (str msg (ascii-char 10))))

	(defmacro debug (n sform form)
		; (debug name sform form)
		`(progn
			(push *debug_return_vals* ,form)
			(debug-format ,n ,sform (env))
			(pop *debug_return_vals*)))

	(defun debug-fun? (form)
		; (debug-fun? form)
		(and (list? form) (nempty? form)
			(not (or (quote? (first form)) (eql 'debug (first form))))
			(sym? (first form)) (setq form (get (first form)))
			(or (func? form) (lambda-func? form))))

	(defun debug-fun (n _)
		; (debug-fun name list) -> list
		(defq s (list _))
		(while (defq l (pop s))
			(each (lambda (e)
				(when (debug-fun? e)
					(elem-set _ l (cat '(debug) (list n (str e) e)))
					(push s e))) l)) _)

	(defmacro defun (n a &rest _)
		`(defq ,n (lambda ,a ~(macroexpand (debug-fun (str n) _)))))
)
