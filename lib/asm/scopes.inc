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

(defun scope-get (_)
	(defq o 0)
	(each! (inc _) -2 (lambda (_)
			(setq o (+ o (elem-get 0 _)))) (list *syms*)) o)

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

(defun scope-def-sym (_1 _2 &rest _3)
	;_1 name
	;_2 type
	;_3 values
	(if (find-rev (setq _1 (sym _1)) (elem-get 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" _1))
	(push (elem-get 1 _) _1)
	(push (elem-get 2 _) (cat (list (length *syms*) _2) _3 '(:nil)))
	(push *syms* _))

(defun scope-get-sym (_1)
	;_1 symbol
	(defq _1 (sym _1) a (some! -2 0 :nil (lambda (s)
		(if (defq _ (find-rev _1 (elem-get 1 s)))
			(elem-get _ (elem-get 2 s)))) (list *syms*)))
	(when a (elem-set -2 a :t)) a)

(defmacro scope-used (&rest _)
	(each scope-get-sym _) :nil)

(defun scope-operator (_1 _2 &optional _3 _4)
	;_1 name
	;_2 precedence
	;_3 associativity
	;_4 compile macro
	(scope-def-sym _1 'op (* _2 2) (opt _3 0) (opt _4 compile-null)))

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
