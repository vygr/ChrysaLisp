;;;;;;;;;;;;;;
; stack scopes
;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;
; variables
;;;;;;;;;;;

(defcvar '*scopes_sp* 0 '*scopes* (list))

(def-enum +scope 0
	(enum offset names types))

(defmacro scope-new ()
	'(push *scopes* (list 0 (list) (list))))

(defun scope-get (level)
	(defq offset 0)
	(each! (inc level) -2
		(# (setq offset (+ offset (elem-get +scope_offset %0))))
		(list *scopes*))
	offset)

(defun push-scope ()
	(setq *scopes_sp* (align *scopes_sp* stack_align))
	(elem-set +scope_offset (elem-get -3 (scope-new)) *scopes_sp*)
	(when (/= *scopes_sp* 0)
		(when *debug_inst*
			(print "(vp-alloc " *scopes_sp* ")"))
		(vp-alloc *scopes_sp*)
		(setq *scopes_sp* 0)))

(defun pop-scope-syms ()
	(pop *scopes*)
	(defq unused (list))
	(each! 0 -1
		(lambda (a)
			(unless (elem-get -2 a)
				(push unused (elem-get _ (elem-get +scope_names scope)))))
		(list (elem-get 2 (defq scope (pop *scopes*)))))
	(if (/= 0 (length unused))
		(throw "Unused symbols !" unused))
	(scope-new)
	(setq *scopes_sp* 0)
	(elem-get +scope_offset scope))

(defun pop-scope ()
	(when (/= (defq offset (pop-scope-syms)) 0)
		(if *debug_inst* (print "(vp-free " offset ")"))
		(vp-free offset)))

(defun pop-scope-checked ()
	(defq offset (pop-scope-syms))
	(when (/= (length *scopes*) 2)
		(throw "Unbalanced scopes !" offset)))

(defun scope-unwind ()
	(when (/= (defq offset (scope-get -1)) 0)
		(if *debug_inst* (print "(vp-free " offset ")"))
		(vp-free offset)))

(defun return ()
	(scope-unwind)
	(if *debug_inst* (print "(vp-ret)"))
	(vp-ret))

(defun scope-def-sym (name type &rest values)
	(if (find-rev (setq name (sym name)) (elem-get +scope_names (defq _ (pop *scopes*))))
		(throw "Symbol redefined !" name))
	(push (elem-get +scope_names _) name)
	(push (elem-get 2 _) (cat (list (length *scopes*) type) values '(:nil)))
	(push *scopes* _))

(defun scope-get-sym (name)
	(defq name (sym name) a (some! -2 0 :nil (lambda (s)
		(if (defq _ (find-rev name (elem-get +scope_names s)))
			(elem-get _ (elem-get 2 s)))) (list *scopes*)))
	(when a (elem-set -2 a :t)) a)

(defmacro scope-used (&rest names)
	(each scope-get-sym names) :nil)

(defun scope-operator (name pres &optional asso impl)
	(scope-def-sym name 'op (* pres 2) (opt asso 0) (opt impl compile-null)))

(defun scope-new-var (type size)
	(push prog
		`(scope-def-sym ',var 'var *scopes_sp* ,type)
		`(setq *scopes_sp* (+ *scopes_sp* ,size))))

(defmacro def-vars (&rest lines)
	; (def-vars [(byte field ...)] ...)
	(defq prog (list progn))
	(each (lambda (line)
		(case (elem-get 0 line)
			(align
				(if (> (length line) 1)
					(push prog `(setq *scopes_sp* (align *scopes_sp* ,(eval (elem-get 1 line)))))
					(push prog '(setq *scopes_sp* (align *scopes_sp* +long_size)))))
			(offset
				(push prog
					`(scope-def-sym ',(elem-get 1 line) 'var *scopes_sp* 0 "")))
			(struct
				(push prog
					`(scope-def-sym ',(elem-get 1 line) 'var *scopes_sp* 0 "")
					`(setq *scopes_sp* (+ *scopes_sp* ,(eval (elem-get 2 line))))))
			(union
				(defq o (gensym) m (gensym))
				(push prog `(defq ,o *scopes_sp* ,m *scopes_sp*))
				(each! 1 -1 (lambda (_)
					(push prog
						`(def-vars ,_)
						`(setq ,m (max ,m *scopes_sp*) *scopes_sp* ,o))) (list line))
				(push prog `(setq *scopes_sp* ,m)))
			(:t
				(if (defq alignment (find (elem-get 0 line)
						'(short int long ushort uint ulong ptr pptr
							pubyte pushort puint pulong pbyte pshort pint plong)))
					(push prog
						`(setq *scopes_sp* (align *scopes_sp* ,(elem-get alignment
							'(+short_size +int_size +long_size +short_size
								+int_size +long_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size))))))
				(each! 1 -1 (lambda (var)
					(case (elem-get 0 line)
						(byte (scope-new-var "b" +byte_size))
						(short (scope-new-var "s" +short_size))
						(int (scope-new-var "i" +int_size))
						(long (scope-new-var "l" +long_size))
						(ubyte (scope-new-var "B" +byte_size))
						(ushort (scope-new-var "S" +short_size))
						(uint (scope-new-var "I" +int_size))
						(ulong (scope-new-var "L" +long_size))
						(ptr (scope-new-var "p" +ptr_size))
						(pptr (scope-new-var "pp" +ptr_size))
						(pubyte (scope-new-var "pB" +ptr_size))
						(pushort (scope-new-var "pS" +ptr_size))
						(puint (scope-new-var "pI" +ptr_size))
						(pulong (scope-new-var "pL" +ptr_size))
						(pbyte (scope-new-var "pb" +ptr_size))
						(pshort (scope-new-var "ps" +ptr_size))
						(pint (scope-new-var "pi" +ptr_size))
						(plong (scope-new-var "pl" +ptr_size))))
					(list line))))) lines)
	prog)

;module
(export-symbols
	scope-operator scope-new scope-get pop-scope-checked
	scope-get-sym scope-def-sym scope-unwind
	def-vars push-scope pop-scope pop-scope-syms scope-used return)
(env-pop)
