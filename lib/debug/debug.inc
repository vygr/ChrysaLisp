;;;;;;;;;;
; Debuging
;;;;;;;;;;

(defq *debug_rvals* (list) *debug_state* :nil)

;module
(env-push)

(defq +debug_width 60)

(structure +debug 0
	(netid reply origin)
	(int type)
	(offset data))

(defun debug-service ()
	(when (/= 0 (length (defq services (mail-enquire "*Debug"))))
		(to-net-id (elem-get 1 (split (pop services) ",")))))

(defun debug-ipc (service type s)
	(mail-send service (setf-> (cat (str-alloc +debug_size) s)
		(+debug_reply (defq mbox (mail-alloc-mbox)))
		(+debug_origin (task-netid))
		(+debug_type type)))
	(setq *debug_state* (mail-read mbox))
	(mail-free-mbox mbox))

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

(defun debug-info (n sform rval e)
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
	(str msg (ascii-char 10)))

(defun debug-msg (n sform rval e)
	(when (defq service (debug-service))
		(debug-ipc service 0 (if (eql *debug_state* ":forward")
			"" (debug-info n sform rval e)))))

(defun debug-brk (name condtion)
	; (debug-brk name condtion)
	(when (defq service (debug-service))
		(debug-ipc service 1 (debug-info (cat "<break> " name) :nil :nil (penv)))))

(defun debug-form? (form)
	(cond
		((and (list? form) (nempty? form) (defq ff (first form))
			(notany (# (eql %0 ff))
				'(profile-report profile-print debug-brk
				quote quasi-quote static-q static-qq)))
			;valid form
			(cond
				((some (# (eql %0 ff)) '(lambda macro const defun defmacro))
					;just step in, but don't wrap
					0)
				((and (sym? ff) (setq form (get ff))
					(or (func? form) (lambda-func? form) (macro-func? form)))
					;step in and wrap
					1)))))

(defun debug-instrument (name form)
	(defq stack (list form))
	(while (defq l (pop stack))
		(each (lambda (e)
			;valid form ?
			(when (defq m (debug-form? e))
				;wrap the form ?
				(when (> m 0)
					(elem-set _ l
						(static-qq (progn
							(push *debug_rvals* ,e)
							(debug-msg ,name ,(str e) (last *debug_rvals*) (env))
							(pop *debug_rvals*)))))
				;step into form
				(push stack e))) l)) form)

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(macroexpand
		(debug-instrument (str n) _))))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a) ~(macroexpand
		(debug-instrument (cat *class* " " n) _))))))

;module
(export-symbols defun defmethod debug-brk)
(env-pop)
