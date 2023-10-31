;;;;;;;;;;
; Debuging
;;;;;;;;;;

(import "apps/debug/app.inc")
(defq *debug_rvals* (list))

;module
(env-push)

(defq +debug_width 60)

(defun debug-trunc (s w &optional d)
	(if (> (length s) (setq w (- w 4)))
		(if d
			(cat "... " (slice (- -1 w) -1 s))
			(cat (slice 0 w s) " ...")) s))

(defun debug-sanitise (s w &optional d)
	(cond
		((eql s "") s)
		(:t (defq q 0 s (apply (const cat) (map (#
				(cond
					((<= 32 (code %0) 126)
						(if (= (code %0) 34) (setq q (inc q)))
						%0)
					(:t "."))) (debug-trunc s w d))))
			(if (odd? q) (cat s (ascii-char 34)) s))))

(defun debug-format (n sform rval e)
	(defq e (tolist e) max_var_size 0)
	(sort (# (cmp (first %0) (first %1))) e)
	(write (defq msg (string-stream (cat "")))
		(cat (defq u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
			(const (cat (ascii-char 10) "; ")) (str n) (const (cat " ;" (ascii-char 10)))
			u (ascii-char 10) (ascii-char 10)
			(debug-sanitise sform +debug_width) (ascii-char 10) (ascii-char 10)
			(debug-sanitise (str rval) +debug_width) (ascii-char 10) (ascii-char 10)))
	(each (lambda (binding)
		(bind '(var val) binding)
		(elem-set 0 binding (setq var (str var)))
		(setq max_var_size (max (length var) max_var_size))) e)
	(each (lambda ((var val))
		(setq var (pad var max_var_size)
			val (debug-sanitise (str val) (- (const (- +debug_width 3)) max_var_size) :t))
		(write msg (cat var " : " val (ascii-char 10)))) e)
	(debug-send (str msg (ascii-char 10))))

(defun debug-fun? (form)
	(cond
		((and (list? form) (nempty? form)
				(not (or (quote? (defq ff (first form))) (quasi-quote? ff))))
			;valid form
			(cond
				((or (lambda? ff) (macro? ff) (eql 'const ff))
					;just step in, but don't wrap
					0)
				((and (sym? ff) (setq form (get ff))
					(or (func? form) (lambda-func? form) (macro-func? form)))
					;step in and wrap
					1)))))

(defun debug-fun (name form)
	(defq stack (list form))
	(while (defq l (pop stack))
		(each (lambda (e)
			;valid form ?
			(when (defq m (debug-fun? e))
				;wrap the form ?
				(when (> m 0)
					(elem-set _ l
						(static-qq (progn
							(push *debug_rvals* ,e)
							(debug-format ,name ,(str e) (last *debug_rvals*) (env))
							(pop *debug_rvals*)))))
				;step into form
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
