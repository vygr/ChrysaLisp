;;;;;;;;;;
; Debuging
;;;;;;;;;;

(when (> *debug_mode* 0)
	(import "apps/debug/app.inc")

	(defq *debug_return_vals* (list))

	(defun debug-trunc (s &optional w)
		(setd w 37)
		(if (> (length s) w)
			(cat (slice 0 w s) " ...") s))

	(defun debug-sanitise (s &optional w)
		(defq s (debug-trunc s w) q 0)
		(cond
			((eql s "") s)
			(:t (setq s (apply (const cat) (map (#
					(cond
						((<= 32 (code %0) 126)
							(if (= (code %0) 34) (setq q (inc q)))
							%0)
						(:t "."))) s)))
				(if (odd? q) (cat s (ascii-char 34)) s))))

	(defun debug-format (n sform rval e)
		; (debug-format name sform rval env)
		(defq e (tolist e) max_var_size 0)
		(sort (# (cmp (first %0) (first %1))) e)
		(write (defq msg (string-stream (cat "")))
			(cat (defq u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
				(const (cat (ascii-char 10) "; ")) (str n) (const (cat " ;" (ascii-char 10)))
				u (ascii-char 10) (ascii-char 10)
				(debug-sanitise sform 56) (ascii-char 10) (ascii-char 10)
				(debug-sanitise (str rval) 56) (ascii-char 10)))
		(each (lambda (binding)
			(bind '(var val) binding)
			(elem-set 0 binding (setq var (str var)))
			(setq max_var_size (max (length var) max_var_size))) e)
		(each (lambda ((var val))
			(setq var (pad var max_var_size) val (debug-sanitise (str val) (- 53 max_var_size)))
			(if (> (length val) 0)
				(setq val (apply (const cat) (map (# (if (<= 32 (code %0) 126) %0 ".")) val))))
			(write msg (cat var " : " val (ascii-char 10)))) e)
		(debug-send (str msg (ascii-char 10))))

	(defmacro debug (n sform form)
		; (debug name sform form)
		`(progn
			(push *debug_return_vals* ,form)
			(debug-format ,n ,sform (last *debug_return_vals*) (env))
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

	(defmacro defmethod (n a &rest _)
		`(def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand (debug-fun (str n) _)))))
)
