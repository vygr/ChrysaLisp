;;;;;;;;;;;;;;
; stack scopes
;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;
; variables
;;;;;;;;;;;

(defcvar '*sym_sp* 0 '*syms* (list))

(defmacro scope-new ()
	'(push *syms* (list 0 (list) (list))))

(defun scope-get (level)
	(defq offset 0)
	(each! (inc level) -2 (# (setq offset (+ offset (elem-get 0 %0)))) (list *syms*))
	offset)

(defun push-scope ()
	(setq *sym_sp* (align *sym_sp* stack_align))
	(elem-set 0 (elem-get -3 (scope-new)) *sym_sp*)
	(when (/= *sym_sp* 0)
		(when *debug_inst*
			(print "(vp-alloc " *sym_sp* ")"))
		(vp-alloc *sym_sp*)
		(setq *sym_sp* 0)))

(defun pop-scope-syms ()
	(pop *syms*)
	(defq u (list))
	(each! 0 -1 (lambda (a)
		(unless (elem-get -2 a) (push u (elem-get _ (elem-get 1 s))))) (list (elem-get 2 (defq s (pop *syms*)))))
	(if (/= 0 (length u))
		(throw "Unused symbols !" u))
	(scope-new)
	(setq *sym_sp* 0)
	(elem-get 0 s))

(defun pop-scope ()
	(when (/= (defq _ (pop-scope-syms)) 0)
		(when *debug_inst*
			(print "(vp-free " _ ")"))
		(vp-free _)))

(defun pop-scope-checked ()
	(defq _ (pop-scope-syms))
	(when (/= (length *syms*) 2)
		(throw "Unbalanced scopes !" _)))

(defun scope-unwind ()
	(when (/= (defq _ (scope-get -1)) 0)
		(when *debug_inst*
			(print "(vp-free " _ ")"))
		(vp-free _)))

(defun return ()
	(scope-unwind)
	(when *debug_inst*
		(print "(vp-ret)"))
	(vp-ret))

(defun scope-def-sym (name type &rest values)
	(if (find-rev (setq name (sym name)) (elem-get 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" name))
	(push (elem-get 1 _) name)
	(push (elem-get 2 _) (cat (list (length *syms*) type) values '(:nil)))
	(push *syms* _))

(defun scope-get-sym (name)
	(defq name (sym name) a (some! -2 0 :nil (lambda (s)
		(if (defq _ (find-rev name (elem-get 1 s)))
			(elem-get _ (elem-get 2 s)))) (list *syms*)))
	(when a (elem-set -2 a :t)) a)

(defmacro scope-used (&rest names)
	(each scope-get-sym names) :nil)

(defun scope-operator (name pres &optional asso impl)
	(scope-def-sym name 'op (* pres 2) (opt asso 0) (opt impl compile-null)))

(defun scope-new-var (type size)
	(push prog
		`(scope-def-sym ',var 'var *sym_sp* ,type)
		`(setq *sym_sp* (+ *sym_sp* ,size))))

(defmacro def-vars (&rest lines)
	; (def-vars [(byte field ...)] ...)
	(defq prog (list progn))
	(each (lambda (line)
		(case (elem-get 0 line)
			(align
				(if (> (length line) 1)
					(push prog `(setq *sym_sp* (align *sym_sp* ,(eval (elem-get 1 line)))))
					(push prog '(setq *sym_sp* (align *sym_sp* +long_size)))))
			(offset
				(push prog
					`(scope-def-sym ',(elem-get 1 line) 'var *sym_sp* 0 "")))
			(struct
				(push prog
					`(scope-def-sym ',(elem-get 1 line) 'var *sym_sp* 0 "")
					`(setq *sym_sp* (+ *sym_sp* ,(eval (elem-get 2 line))))))
			(union
				(defq o (gensym) m (gensym))
				(push prog `(defq ,o *sym_sp* ,m *sym_sp*))
				(each! 1 -1 (lambda (_)
					(push prog
						`(def-vars ,_)
						`(setq ,m (max ,m *sym_sp*) *sym_sp* ,o))) (list line))
				(push prog `(setq *sym_sp* ,m)))
			(:t
				(if (defq alignment (find (elem-get 0 line)
						'(short int long ushort uint ulong ptr pptr
							pubyte pushort puint pulong pbyte pshort pint plong)))
					(push prog
						`(setq *sym_sp* (align *sym_sp* ,(elem-get alignment
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
