;;;;;;;;;;;;;;
; stack scopes
;;;;;;;;;;;;;;

;module
(env-push)

;;;;;;;;;;;
; variables
;;;;;;;;;;;

(defcvar '*scopes_sp* 0 '*scopes* (list))

(defmacro scope-new ()
	(static-q (push *scopes* (list 0 (env 1)))))

(defun scope-offset (level)
	(reduce! (# (+ %0 (first %1))) (list *scopes*) 0 (inc level) -2))

(defun push-scope ()
	(elem-set (elem-get (scope-new) -3) 0
		(setq *scopes_sp* (align *scopes_sp* stack_align)))
	(when (/= *scopes_sp* 0)
		(if *debug_inst* (print "(vp-alloc " *scopes_sp* ")"))
		(vp-alloc *scopes_sp*)
		(setq *scopes_sp* 0)))

(defun pop-scope-syms ()
	(pop *scopes*)
	(if (nempty? (defq unused
			(reduce (lambda (%0 (k v)) (if (last v) %0 (push %0 k)))
				(tolist (last (defq scope (pop *scopes*)))) (list))))
		(throw "Unused symbols !" unused))
	(scope-new)
	(setq *scopes_sp* 0)
	(first scope))

(defun pop-scope ()
	(when (/= (defq offset (pop-scope-syms)) 0)
		(if *debug_inst* (print "(vp-free " offset ")"))
		(vp-free offset)))

(defun pop-scope-checked ()
	(defq offset (pop-scope-syms))
	(when (/= (length *scopes*) 2)
		(throw "Unbalanced scopes !" offset)))

(defun scope-unwind ()
	(when (/= (defq offset (scope-offset -1)) 0)
		(if *debug_inst* (print "(vp-free " offset ")"))
		(vp-free offset)))

(defun return ()
	(scope-unwind)
	(if *debug_inst* (print "(vp-ret)"))
	(vp-ret))

(defun scope-def (name type &rest attribs)
	(if (def? (setq name (sym name)) (defq vars (last (defq scope (pop *scopes*)))))
		(throw "Symbol redefined !" name))
	(def vars name (cat (list (length *scopes*) type) attribs '(:nil)))
	(push *scopes* scope))

(defun scope-get (name)
	(if (defq name (sym name) attrib
			(some! (# (def? name (last %0))) (list *scopes*) :nil -2 0))
		(elem-set attrib -2 :t)))

(defmacro scope-used (&rest names)
	(each scope-get names) :nil)

(defun scope-operator (name pres &optional asso impl)
	(scope-def name 'op (* pres 2) (ifn asso 0) (ifn impl compile-null)))

(defun scope-new-var (type size)
	(push prog
		`(scope-def ',var 'var *scopes_sp* ,type)
		`(setq *scopes_sp* (+ *scopes_sp* ,size))))

(defmacro def-vars (&rest lines)
	; (def-vars [(byte field ...)] ...)
	(defq prog (list progn))
	(each (lambda (line)
		(case (first line)
			(align
				(if (> (length line) 1)
					(push prog `(setq *scopes_sp* (align *scopes_sp* ,(eval (second line)))))
					(push prog '(setq *scopes_sp* (align *scopes_sp* +long_size)))))
			(offset
				(push prog
					`(scope-def ',(second line) 'var *scopes_sp* 0 "")))
			(struct
				(push prog
					`(scope-def ',(second line) 'var *scopes_sp* 0 "")
					`(setq *scopes_sp* (+ *scopes_sp* ,(eval (third line))))))
			(union
				(defq o (gensym) m (gensym))
				(push prog `(defq ,o *scopes_sp* ,m *scopes_sp*))
				(each! (lambda (_)
					(push prog
						`(def-vars ,_)
						`(setq ,m (max ,m *scopes_sp*) *scopes_sp* ,o))) (list line) 1)
				(push prog `(setq *scopes_sp* ,m)))
			(:t
				(if (defq alignment (find (first line)
						'(short int long ushort uint long ptr pptr
							pubyte pushort puint pbyte pshort pint plong)))
					(push prog
						`(setq *scopes_sp* (align *scopes_sp* ,(elem-get
							'(+short_size +int_size +long_size +short_size
								+int_size +long_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size)
							alignment)))))
				(each! (lambda (var)
					(case (first line)
						(byte (scope-new-var "b" +byte_size))
						(short (scope-new-var "s" +short_size))
						(int (scope-new-var "i" +int_size))
						(long (scope-new-var "l" +long_size))
						(ubyte (scope-new-var "B" +byte_size))
						(ushort (scope-new-var "S" +short_size))
						(uint (scope-new-var "I" +int_size))
						(ptr (scope-new-var "p" +ptr_size))
						(pptr (scope-new-var "pp" +ptr_size))
						(pubyte (scope-new-var "pB" +ptr_size))
						(pushort (scope-new-var "pS" +ptr_size))
						(puint (scope-new-var "pI" +ptr_size))
						(pbyte (scope-new-var "pb" +ptr_size))
						(pshort (scope-new-var "ps" +ptr_size))
						(pint (scope-new-var "pi" +ptr_size))
						(plong (scope-new-var "pl" +ptr_size))))
					(list line) 1)))) lines)
	prog)

;module
(export-symbols
	'(scope-operator scope-new scope-offset pop-scope-checked
	scope-get scope-def scope-unwind
	def-vars push-scope pop-scope pop-scope-syms scope-used return))
(env-pop)
