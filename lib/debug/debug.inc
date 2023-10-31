;;;;;;;;;;
; Debuging
;;;;;;;;;;

(import "apps/debug/app.inc")
(defq *debug_rvals* (list))

;module
(env-push)

(defun debug-trunc (s w)
	(if (> (length s) w)
		(cat (slice 0 w s) " ...") s))

(defun debug-sanitise (s w)
	(cond
		((eql s "") s)
		(:t (defq q 0 s (apply (const cat) (map (#
				(cond
					((<= 32 (code %0) 126)
						(if (= (code %0) 34) (setq q (inc q)))
						%0)
					(:t "."))) (debug-trunc s w))))
			(if (odd? q) (cat s (ascii-char 34)) s))))

(defun debug-format (n sform rval e)
	(defq e (tolist e) max_var_size 0)
	(sort (# (cmp (first %0) (first %1))) e)
	(write (defq msg (string-stream (cat "")))
		(cat (defq u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
			(const (cat (ascii-char 10) "; ")) (str n) (const (cat " ;" (ascii-char 10)))
			u (ascii-char 10) (ascii-char 10)
			(debug-sanitise sform 56) (ascii-char 10) (ascii-char 10)
			(debug-sanitise (str rval) 56) (ascii-char 10) (ascii-char 10)))
	(each (lambda (binding)
		(bind '(var val) binding)
		(elem-set 0 binding (setq var (str var)))
		(setq max_var_size (max (length var) max_var_size))) e)
	(each (lambda ((var val))
		(setq var (pad var max_var_size) val (debug-sanitise (str val) (- 53 max_var_size)))
		(write msg (cat var " : " val (ascii-char 10)))) e)
	(debug-send (str msg (ascii-char 10))))

(defun debug-fun? (form)
	(and (list? form) (nempty? form)
		(not (or (quote? (first form)) (quasi-quote? (first form))))
		(sym? (first form)) (setq form (get (first form)))
		(or (func? form) (lambda-func? form))))

(defun debug-fun (name form)
	(defq stack (list form))
	(while (defq l (pop stack))
		(each (lambda (e)
			(when (debug-fun? e)
				(elem-set _ l
					(static-qq (progn
						(push *debug_rvals* ,e)
						(debug-format ,name ,(str e) (last *debug_rvals*) (env))
						(pop *debug_rvals*))))
				(push stack e))) l)) form)

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(debug-fun (str n) _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(debug-fun (cat *class* " " n) _))))))

;module
(export-symbols defun defmethod)
(env-pop)
