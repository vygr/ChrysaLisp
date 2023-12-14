;;;;;;;;;;
; Debuging
;;;;;;;;;;

;module
(env-push)

(defq +debug_width 60 +LF (ascii-char 10))

(structure +debug 0
	(netid reply origin)
	(int type)
	(offset data))

(defun debug-service ()
	(when (and (not (eql *debug_state* :gui))
			(/= 0 (length (defq services (mail-enquire "*Debug")))))
		(to-net-id (second (split (pop services) ",")))))

(defun debug-ipc (service type s)
	(mail-send service (setf-> (cat (str-alloc +debug_size) s)
		(+debug_reply (defq mbox (mail-alloc-mbox)))
		(+debug_origin (task-netid))
		(+debug_type type)))
	(setq *debug_state* (mail-read mbox))
	(mail-free-mbox mbox))

(defun debug-trunc (s w &optional d)
	(if (> (length s) w)
		(if d
			(cat "... " (slice (- 3 w) -1 s))
			(cat (slice 0 (- w 4) s) " ...")) s))

(defun debug-sanitize (s w &optional d)
	(if (eql s "") s
		(apply (const cat) (map
			(# (if (<= 0x20 (code %0) 0x7e)
				(if (find 0 %0 "{}") "." %0) "."))
			(debug-trunc s w d)))))

(defun debug-info (rval sform e n)
	(defq e (tolist e) max_var_size 0
		msg (cap (+ (* (length e) 4) 16) (list))
		u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
	(push msg u +LF
		"; " n " ;" +LF
		u +LF +LF
		(debug-sanitize sform +debug_width) +LF +LF
		(debug-sanitize (str rval) +debug_width) +LF +LF)
	(each (lambda ((var val))
		(setq max_var_size (max (length var) max_var_size))) e)
	(each (lambda ((var val))
		(setq var (pad var max_var_size)
			val (debug-sanitize (str val) (- (const (- +debug_width 3)) max_var_size) :t))
		(push msg var " : " val +LF)) (sort (# (cmp (first %0) (first %1))) e))
	(apply (const cat) (push msg +LF)))

(defun debug-msg (rval sform e n)
	(when (defq service (debug-service))
		(debug-ipc service 0 (if (eql *debug_state* ":forward")
			"" (debug-info rval sform e n))))
	rval)

(defun debug-brk (name condition)
	(and condition (defq service (debug-service))
		(debug-ipc service 1 (debug-info :nil :nil (penv) (cat "<break> " name)))))

(defun debug-form? (form)
	(cond
		((and (list? form) (nempty? form) (defq ff (first form))
			(notany (# (eql %0 ff))
				'(profile-instrument profile-report debug-msg
				stack-instrument debug-brk debug-instrument
				quote quasi-quote static-q static-qq const
				defmethod)))
			;valid form
			(cond
				((or (list? ff) (some (# (eql %0 ff))
						'(lambda macro # defun defmacro)))
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
				(when (> m 0) (elem-set _ l
					(static-qq (debug-msg ,e ,(str e) (env) ,name))))
				;step into form
				(push stack e))) l)) form)

(defmacro defun (n a &rest _)
	(static-qq (defq ,n (lambda ,a ~(debug-instrument (str n) _)))))

(defmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (lambda (this ~a)
		~(debug-instrument (cat *class* " " n) _)))))

;module
(export-symbols
	'(defun defmethod debug-brk))
(env-pop)
