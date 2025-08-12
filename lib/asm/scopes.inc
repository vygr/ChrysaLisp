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
	(enum offset names attribs))

;last attrib entry is for used flag !
(defq +attrib_used -2)

(defmacro scope-new ()
	(static-q (push *scopes* (list 0 (list) (list)))))

(defun scope-get (level)
	(defq offset 0)
	(each! (# (setq offset (+ offset (elem-get %0 +scope_offset))))
		(list *scopes*) (inc level) -2)
	offset)

(defun push-scope ()
	(setq *scopes_sp* (align *scopes_sp* stack_align))
	(elem-set (elem-get (scope-new) -3) +scope_offset *scopes_sp*)
	(when (/= *scopes_sp* 0)
		(if *debug_inst* (print "(vp-alloc " *scopes_sp* ")"))
		(vp-alloc *scopes_sp*)
		(setq *scopes_sp* 0)))

(defun pop-scope-syms ()
	(pop *scopes*)
	(defq unused (list))
	(each! (lambda (attrib)
		(unless (elem-get attrib +attrib_used)
			(push unused (elem-get (elem-get scope +scope_names) (!)))))
		(list (elem-get (defq scope (pop *scopes*)) +scope_attribs)))
	(if (/= 0 (length unused))
		(throw "Unused symbols !" unused))
	(scope-new)
	(setq *scopes_sp* 0)
	(elem-get scope +scope_offset))

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

(defun scope-def-sym (name type &rest attribs)
	(if (rfind (setq name (sym name))
			(elem-get (defq scope (pop *scopes*)) +scope_names))
		(throw "Symbol redefined !" name))
	(push (elem-get scope +scope_names) name)
	(push (elem-get scope +scope_attribs) (cat (list (length *scopes*) type) attribs '(:nil)))
	(push *scopes* scope))

(defun scope-get-sym (name)
	(defq name (sym name) attrib (some! (lambda (scope)
		(if (defq _ (rfind name (elem-get scope +scope_names)))
			(elem-get (elem-get scope +scope_attribs) _))) (list *scopes*) :nil -2 0))
	(when attrib (elem-set attrib +attrib_used :t)) attrib)

(defmacro scope-used (&rest names)
	(each scope-get-sym names) :nil)

(defun scope-operator (name pres &optional asso impl)
	(scope-def-sym name 'op (* pres 2) (ifn asso 0) (ifn impl compile-null)))

(defun scope-new-var (type size)
	(push prog
		`(scope-def-sym ',var 'var *scopes_sp* ,type)
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
					`(scope-def-sym ',(second line) 'var *scopes_sp* 0 "")))
			(struct
				(push prog
					`(scope-def-sym ',(second line) 'var *scopes_sp* 0 "")
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
						'(short int long ushort uint ulong ptr pptr
							pubyte pushort puint pulong pbyte pshort pint plong)))
					(push prog
						`(setq *scopes_sp* (align *scopes_sp* ,(elem-get
							'(+short_size +int_size +long_size +short_size
								+int_size +long_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size)
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
					(list line) 1)))) lines)
	prog)

;module
(export-symbols
	'(scope-operator scope-new scope-get pop-scope-checked
	scope-get-sym scope-def-sym scope-unwind
	def-vars push-scope pop-scope pop-scope-syms scope-used return))
(env-pop)
