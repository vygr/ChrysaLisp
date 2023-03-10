;;;;;;;;;;;;;;;;;;;
; structured coding
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defcvar '*switch* :nil '*switch_nxt* :nil '*switch_stk* (list))

(defmacro beg-sym () '(sym (str "b_" *switch*)))
(defmacro end-sym () '(sym (str "e_" *switch*)))
(defmacro loc-sym (n) `(sym (str "o_" *switch* "_" ,n)))
(defmacro cnt-sym () '(sym (str "_c_" *switch*)))
(defmacro llb-sym (_) `(sym (cat "_b_" ,_)))
(defmacro lle-sym (_) `(sym (cat "_e_" ,_)))

(defun goto (l)
	(vp-jmp l))

(defun gotoif (e l)
	(bind '(d o s) (cond
		((str? e) (assign e '(:r0)) '(:r0 /= 0))
		(e)))
	(defq e (find-rev o '(= /= <= >= > <)))
	(case (assign-src-type s)
		(:c (if e ((eval (elem-get e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem-get e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(:t (throw "No such compare mode" (assign-src-type s)))))

(defun gotoifnot (e l)
	(bind '(d o s) (cond
		((str? e) (assign e '(:r0)) '(:r0 /= 0))
		(e)))
	(defq e (find-rev o '(/= = > < <= >=)))
	(case (assign-src-type s)
		(:c (if e ((eval (elem-get e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem-get e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(:t (throw "No such compare mode" (assign-src-type s)))))

(defun switch (&optional _)
	(push *switch_stk* *switch*)
	(setq *switch* *switch_nxt* *switch_nxt* (inc *switch_nxt*))
	(when _ (deffvar (llb-sym _) (beg-sym) (lle-sym _) (end-sym)))
	(deffvar (cnt-sym) 0))

(defun default ()
	(vp-label (loc-sym (defq s (cnt-sym) c (eval s))))
	(set (env) s (inc c)))

(defun endswitch ()
	(vp-label (end-sym))
	(default)
	(setq *switch* (pop *switch_stk*)))

(defun nextcaseif (e)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defun nextcaseifnot (e)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defun exitif (e)
	(gotoif e (if l (eval (lle-sym l)) (end-sym))))

(defun exitifnot (e)
	(gotoifnot e (if l (eval (lle-sym l)) (end-sym))))

(defun repeatif (e)
	(gotoif e (if l (eval (llb-sym l)) (beg-sym))))

(defun repeatifnot (e)
	(gotoifnot e (if l (eval (llb-sym l)) (beg-sym))))

(defun break (&optional l)
	(goto (if l (eval (lle-sym l)) (end-sym))))

(defun continue (&optional l)
	(goto (if l (eval (llb-sym l)) (beg-sym))))

(defun breakif (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i exitif (list e)))

(defun breakifnot (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i exitifnot (list e)))

(defun continueif (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i repeatif (list e)))

(defun continueifnot (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i repeatifnot (list e)))

(defun vpcase (&rest e)
	(default)
	(each! 0 -1 nextcaseifnot (list e)))

(defun vpcasenot (&rest e)
	(default)
	(each! 0 -1 nextcaseif (list e)))

(defun loop-start (&optional l)
	(switch l)
	(vp-label (beg-sym)))

(defun loop-while (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(switch l)
	(vp-label (beg-sym))
	(each! 0 i exitifnot (list e)))

(defun loop-whilenot (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(switch l)
	(vp-label (beg-sym))
	(each! 0 i exitif (list e)))

(defun loop-end ()
	(continue)
	(endswitch))

(defun loop-until (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i repeatifnot (list e))
	(endswitch))

(defun loop-untilnot (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(each! 0 i repeatif (list e))
	(endswitch))

(defun vpif (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(switch l)
	(each! 0 i nextcaseifnot (list e)))

(defun vpifnot (&rest e)
	(unless (sym? (defq i -2 l (elem-get -2 e)))
		(setq i -1 l :nil))
	(switch l)
	(each! 0 i nextcaseif (list e)))

(defun else ()
	(break)
	(default))

(defun elseif (&rest e)
	(else)
	(each! 0 -1 nextcaseifnot (list e)))

(defun elseifnot (&rest e)
	(else)
	(each! 0 -1 nextcaseif (list e)))

(defun endif ()
	(endswitch))

;;;;;;;;;;;
; errorcase
;;;;;;;;;;;

(defmacro errorcases (&rest e) (if (> *debug_mode* 0) `(progn ~e)))
(defmacro noterrorcases (&rest e) (if (<= *debug_mode* 0) `(progn ~e)))
(defmacro errorif (e l) `(errorcases (gotoif ,e ,l)))
(defmacro errorifnot (e l) `(errorcases (gotoifnot ,e ,l)))

;;;;;;;
; enums
;;;;;;;

(defun def-enum (size)
	(push syms (sym (cat name "_" field)))
	(push values (num-intern base))
	(setq base (+ base size)))

(defmacro def-enum (name base &rest lines)
	; (def-enum name base [(enum field ...)] ...)
	(if (def? (sym (cat name "_size")) *compile_env*) (throw "Enum redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! 1 -1 (lambda (field)
			(case (elem-get 0 line)
				(enum (def-enum +byte_size))))
			(list line))) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	`(eval '(bind ',syms ',values) *compile_env*))

;;;;;;
; bits
;;;;;;

(defun def-bit ()
	(push syms (sym (cat name "_" field)))
	(push values (num-intern (<< 1 base)))
	(setq base (inc base)))

(defmacro def-bit (name base &rest lines)
	; (def-bit name base [(bit field ...)] ...)
	(if (def? (sym (cat name "_size")) *compile_env*) (throw "Bit redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! 1 -1 (lambda (field)
			(case (elem-get 0 line)
				(bit (def-bit))))
			(list line))) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	`(eval '(bind ',syms ',values) *compile_env*))

;;;;;;;;;;;;
; structures
;;;;;;;;;;;;

(defmacro def-struct (name base &rest lines)
	; (def-struct name base [(byte field ...)] ...)
	(unless (eql (cat name "_size") "local_size")
		(if (def? (sym (cat name "_size")) *compile_env*) (throw "Structure redefined !" name)))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(setq base (_structure base line))) lines)
	(push syms (sym (cat name "_size")))
	(push values (num-intern base))
	`(eval '(bind ',syms ',values) *compile_env*))

;;;;;;;;;;;
; variables
;;;;;;;;;;;

(defcvar '*sym_sp* 0 '*syms* (list))

(defmacro new-scope ()
	'(push *syms* (list 0 (list) (list))))

(defun get-scope (_)
	(defq o 0)
	(each! (inc _) -2 (lambda (_)
			(setq o (+ o (elem-get 0 _)))) (list *syms*)) o)

(defun push-scope ()
	(setq *sym_sp* (align *sym_sp* stack_align))
	(elem-set 0 (elem-get -3 (new-scope)) *sym_sp*)
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
	(new-scope)
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

(defun unwind ()
	(when (/= (defq _ (get-scope -1)) 0)
		(when *debug_inst*
			(print "(vp-free " _ ")"))
		(vp-free _)))

(defun return ()
	(unwind)
	(when *debug_inst*
		(print "(vp-ret)"))
	(vp-ret))

(defun def-sym (_1 _2 &rest _3)
	;_1 name
	;_2 type
	;_3 values
	(if (find-rev (setq _1 (sym _1)) (elem-get 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" _1))
	(push (elem-get 1 _) _1)
	(push (elem-get 2 _) (cat (list (length *syms*) _2) _3 '(:nil)))
	(push *syms* _))

(defun get-sym (_1)
	;_1 symbol
	(defq _1 (sym _1) a (some! -2 0 :nil (lambda (s)
		(if (defq _ (find-rev _1 (elem-get 1 s)))
			(elem-get _ (elem-get 2 s)))) (list *syms*)))
	(when a (elem-set -2 a :t)) a)

(defmacro used-syms (&rest _)
	(each get-sym _) :nil)

(defun operator (_1 _2 &optional _3 _4)
	;_1 name
	;_2 precedence
	;_3 associativity
	;_4 compile macro
	(def-sym _1 'op (* _2 2) (opt _3 0) (opt _4 compile-null)))

(defmacro def-vars (&rest lines)
	; (def-vars [(byte field ...)] ...)
	(defq prog (list progn))
	(defun new-var (type size)
		(push prog
			`(def-sym ',var 'var *sym_sp* ,type)
			`(setq *sym_sp* (+ *sym_sp* ,size))))
	(each (lambda (line)
		(case (elem-get 0 line)
			(align
				(if (> (length line) 1)
					(push prog `(setq *sym_sp* (align *sym_sp* ,(eval (elem-get 1 line)))))
					(push prog '(setq *sym_sp* (align *sym_sp* +long_size)))))
			(offset
				(push prog
					`(def-sym ',(elem-get 1 line) 'var *sym_sp* 0 "")))
			(struct
				(push prog
					`(def-sym ',(elem-get 1 line) 'var *sym_sp* 0 "")
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
						(byte (new-var "b" +byte_size))
						(short (new-var "s" +short_size))
						(int (new-var "i" +int_size))
						(long (new-var "l" +long_size))
						(ubyte (new-var "B" +byte_size))
						(ushort (new-var "S" +short_size))
						(uint (new-var "I" +int_size))
						(ulong (new-var "L" +long_size))
						(ptr (new-var "p" +ptr_size))
						(pptr (new-var "pp" +ptr_size))
						(pubyte (new-var "pB" +ptr_size))
						(pushort (new-var "pS" +ptr_size))
						(puint (new-var "pI" +ptr_size))
						(pulong (new-var "pL" +ptr_size))
						(pbyte (new-var "pb" +ptr_size))
						(pshort (new-var "ps" +ptr_size))
						(pint (new-var "pi" +ptr_size))
						(plong (new-var "pl" +ptr_size))))
					(list line))))) lines)
	prog)

;module
(export-symbols
	def-vars def-enum def-struct def-bit operator
	new-scope get-scope push-scope pop-scope pop-scope-syms pop-scope-checked
	errorcases noterrorcases errorif errorifnot
	switch endswitch vpcase vpcasenot default nextcaseif nextcaseifnot
	vpif vpifnot else elseif elseifnot endif
	break breakif breakifnot goto gotoif gotoifnot exitif exitifnot
	loop-start loop-end loop-while loop-whilenot loop-until loop-untilnot
	continue continueif continueifnot repeatif repeatifnot
	used-syms get-sym unwind return)
(env-pop)

;;;;;;;;;;;;;;;;;;;;;;;;
; assignment and cscript
;;;;;;;;;;;;;;;;;;;;;;;;

(include "lib/asm/assign.inc")
