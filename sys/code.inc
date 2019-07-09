;;;;;;;;;;;;;;;;;;;
; structured coding
;;;;;;;;;;;;;;;;;;;

(defcvar '*switch* nil '*switch-nxt* nil '*switch-stk* (list))

(defmacro beg-sym ()
	'(sym (str "b_" *switch*)))

(defmacro end-sym ()
	'(sym (str "e_" *switch*)))

(defmacro loc-sym (n)
	`(sym (str "o_" *switch* "_" ,n)))

(defmacro cnt-sym ()
	'(sym (str "_c_" *switch*)))

(defmacro llb-sym (_)
	`(sym (cat "_s_" _)))

(defcfun goto (l)
	(vp-jmp l))

(defcfun gotoif (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 != 0)))
	(bind '(d o s) e)
	(defq e (find o '(== != <= >= > <)))
	(case (vp-type s)
		(c (if e ((eval (elem e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(r (if e ((eval (elem e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(otherwise (throw "No such compare mode" (vp-type s)))))

(defcfun gotoifnot (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 != 0)))
	(bind '(d o s) e)
	(defq e (find o '(!= == > < <= >=)))
	(case (vp-type s)
		(c (if e ((eval (elem e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(r (if e ((eval (elem e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(otherwise (throw "No such compare mode" (vp-type s)))))

(defcmacro errorif (e l)
	(if debug_mode (list 'gotoif e l)))

(defcmacro errorifnot (e l)
	(if debug_mode (list 'gotoifnot e l)))

(defcmacro errorcases (&rest e)
	(if debug_mode (cat '(progn) e)))

(defcfun switch (&optional _)
	(push *switch-stk* *switch*)
	(setq *switch* *switch-nxt* *switch-nxt* (inc *switch-nxt*))
	(when _
		(defcvar (setq _ (llb-sym _)) (end-sym))
		(push *func-syms* _))
	(defcvar (setq _ (cnt-sym)) 0)
	(push *func-syms* _))

(defcfun default ()
	(vp-label (loc-sym (defq s (cnt-sym) c (eval s))))
	(set (env) s (inc c)))

(defcfun endswitch ()
	(vp-label (end-sym))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcfun vpcase (e)
	(default)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defcfun vpcasenot (e)
	(default)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defcfun break (&optional _)
	(goto (if _ (eval (llb-sym _)) (end-sym))))

(defcfun breakif (e &optional _)
	(gotoif e (if _ (eval (llb-sym _)) (end-sym))))

(defcfun breakifnot (e &optional _)
	(gotoifnot e (if _ (eval (llb-sym _)) (end-sym))))

(defcfun continue ()
	(goto (beg-sym)))

(defcfun continueif (e)
	(gotoif e (beg-sym)))

(defcfun continueifnot (e)
	(gotoifnot e (beg-sym)))

(defcfun loop-start (&optional _)
	(switch _)
	(vp-label (beg-sym)))

(defcfun loop-while (e &optional _)
	(switch _)
	(vp-label (beg-sym))
	(breakifnot e))

(defcfun loop-whilenot (e &optional _)
	(switch _)
	(vp-label (beg-sym))
	(breakif e))

(defcfun loop-end ()
	(continue)
	(endswitch))

(defcfun loop-until (e)
	(continueifnot e)
	(endswitch))

(defcfun loop-untilnot (e)
	(continueif e)
	(endswitch))

(defcfun vpif (e &optional _)
	(switch _)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defcfun vpifnot (e &optional _)
	(switch _)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defcfun elseif (e)
	(break)
	(vpcase e))

(defcfun elseifnot (e)
	(break)
	(vpcasenot e))

(defcfun else ()
	(break)
	(default))

(defcfun endif ()
	(endswitch))

;;;;;;;
; enums
;;;;;;;

(defcfun def-enum (_1 &optional _2)
	(defcvar '*enum* _1 '*enum-offset* (opt _2 0)))

(defcfun def-enum-end ()
	(undefc '*enum* '*enum-offset*))

(defcfun enum (&rest _)
	(each (lambda (_)
		(if (def? (setq _ (sym (cat *enum* "_" _))))
			(throw "Enum redefined !" _))
		(defcvar _ *enum-offset* '*enum-offset* (inc *enum-offset*))) _))

;;;;;;
; bits
;;;;;;

(defcfun def-bit (_1 &optional _2)
	(defcvar '*bit* _1 '*bit-offset* (opt _2 0)))

(defcfun def-bit-end ()
	(undefc '*bit* '*bit-offset*))

(defcfun bit (&rest _)
	(each (lambda (_)
		(if (def? (setq _ (sym (cat *bit* "_" _))))
			(throw "Bit redefined !" _))
		(defcvar _ (shl 1 *bit-offset*) '*bit-offset* (inc *bit-offset*))) _))

;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

(defcvar '*sym-sp* 0 '*syms* (list))

(defmacro new-scope ()
	'(push *syms* (list 0 (list) (list))))

(defcfun get-scope (_)
	(defq o 0)
	(each! (inc _) -2 nil (lambda (_)
			(setq o (add o (elem 0 _)))) (list *syms*)) o)

(defcfun push-scope ()
	(local-align stack_align)
	(elem-set 0 (elem -3 (new-scope)) *sym-sp*)
	(when (ne *sym-sp* 0)
		(when debug_inst
			(print "(vp-alloc " *sym-sp* ")"))
		(vp-alloc *sym-sp*)
		(setq *sym-sp* 0)))

(defcfun pop-scope-syms ()
	(pop *syms*)
	(defq u (list))
	(each! nil nil nil (lambda (a)
		(unless (elem -2 a) (push u (elem _ (elem 1 s))))) (list (elem 2 (defq s (pop *syms*)))))
	(if (ne 0 (length u))
		(throw "Unused symbols !" u))
	(new-scope)
	(setq *sym-sp* 0)
	(elem 0 s))

(defcfun pop-scope ()
	(when (ne (defq _ (pop-scope-syms)) 0)
		(when debug_inst
			(print "(vp-free " _ ")"))
		(vp-free _)))

(defcfun pop-scope-checked ()
	(defq _ (pop-scope-syms))
	(when (ne (length *syms*) 2)
		(throw "Unbalanced scopes !" _)))

(defcfun unwind ()
	(when (ne (defq _ (get-scope -1)) 0)
		(when debug_inst
			(print "(vp-free " _ ")"))
		(vp-free _)))

(defcfun return ()
	(unwind)
	(when debug_inst
		(print "(vp-ret)"))
	(vp-ret))

(defcfun def-sym (_1 _2 &rest _3)
	;_1 name
	;_2 type
	;_3 values
	(if (find (setq _1 (sym _1)) (elem 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" _1))
	(push (elem 1 _) _1)
	(push (elem 2 _) (cat (list (length *syms*) _2) _3 '(nil)))
	(push *syms* _))

(defcfun get-sym (_1)
	;_1 name
	(defq _1 (sym _1) a (some! -2 0 t (lambda (s)
		(if (defq _ (find _1 (elem 1 s)))
			(elem _ (elem 2 s)))) (list *syms*)))
	(when a (elem-set -2 a t)) a)

(defcfun used-syms (&rest _)
	(each get-sym _))

(defcfun operator (_1 _2 &optional _3 _4)
	;_1 name
	;_2 precedence
	;_3 associativity
	;_4 compile macro
	(def-sym _1 'op (mul _2 2) (opt _3 0) (opt _4 compile-null)))

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

(defcvar '*struct* nil '*struct-sp* nil
	'null_size 0 'byte_size 1 'short_size 2 'int_size 4 'long_size 8 'ptr_size 8)

(defmacro type-sym (_)
	`(sym (str "_t_" ,_)))

(defmacro struct-sym (_)
	`(sym (cat *struct* "_" ,_)))

(defcfun check-field (_)
	(setq _ (struct-sym _))
	(unless (eql *struct* 'local)
		(if (def? _)
			(throw "Field redefined !" _))) _)

(defcfun local-align (&optional _)
	(setd _ ptr_size)
	(if *struct*
		(setq *struct-sp* (align *struct-sp* _))
		(setq *sym-sp* (align *sym-sp* _))))

(defcfun def-struct (s &optional o)
	(setq *struct* s *struct-sp* (eval (sym (cat (opt o "null") "_size")))))

(defcfun def-struct-end ()
	(defcvar (sym (cat *struct* "_size")) *struct-sp* '*struct* nil '*struct-sp* nil))

(defmacro def-type (n s y)
	`(defcfun ,n (&rest f)
		(each (lambda (_)
			(local-align ,s)
			(cond
				(*struct*
					(defcvar (setq _ (check-field _)) *struct-sp* (type-sym _) ,y
						'*struct-sp* (add *struct-sp* ,s)))
				(t
					(def-sym _ 'var *sym-sp* ,y)
					(setq *sym-sp* (add *sym-sp* ,s))))) f)))

(def-type byte byte_size "b")
(def-type ubyte byte_size "B")
(def-type short short_size "s")
(def-type ushort short_size "S")
(def-type int int_size "i")
(def-type uint int_size "I")
(def-type long long_size "l")
(def-type ulong long_size "L")
(def-type ptr ptr_size "p")
(def-type pbyte ptr_size "pb")
(def-type pubyte ptr_size "pB")
(def-type pshort ptr_size "ps")
(def-type pushort ptr_size "pS")
(def-type pint ptr_size "pi")
(def-type puint ptr_size "pI")
(def-type plong ptr_size "pl")
(def-type pulong ptr_size "pL")
(def-type pptr ptr_size "pp")

(defcfun offset (_)
	(cond
		(*struct*
			(defcvar (check-field _) *struct-sp*))
		(t
			(throw "Attempt to define offset into local stack !" _))))

(defcfun struct (_ s)
	(if (sym? s) (setq s (eval (sym (cat s "_size")))))
	(cond
		(*struct*
			(defcvar (setq _ (check-field _)) *struct-sp* (type-sym _) nil
				'*struct-sp* (add *struct-sp* s)))
		(t
			(def-sym _ 'var *sym-sp* 0 "")
			(setq *sym-sp* (add *sym-sp* s)))))

(defcfun union (&rest _)
	(if (notevery lst? _)
		(throw "Union needs lists as parameters !" _))
	(cond
		(*struct*
			(defq o *struct-sp* m o)
			(each (lambda (_)
				(setq *struct-sp* o)
				(eval _)
				(setq m (max m *struct-sp*))) _)
			(setq *struct-sp* m))
		(t
			(defq o *sym-sp* m o)
			(each (lambda (_)
				(setq *sym-sp* o)
				(eval _)
				(setq m (max m *sym-sp*))) _)
			(setq *sym-sp* m))))

;;;;;;;;;;;;;;;;;;;;
; parameter handling
;;;;;;;;;;;;;;;;;;;;

(defcfun assign-ignored-to-asm ()
	(defq s (list) d (list))
	(each! nil nil nil (lambda (x y)
		(unless (or (eql x y) (eql y '_))
			(push s x) (push d y))) (list *src* *dst*))
	(setq *src* s *dst* d))

(defcfun assign-ignored-to-script ()
	(defq s (list) d (list))
	(each! nil nil nil (lambda (x y)
		(unless (eql y "_")
			(push s x) (push d y))) (list *src* *dst*))
	(setq *src* s *dst* d))

(defcfun assign-match-any (d s)
	(cond
		((defq j (lst? s) i (lst? d))
			(cond
				(j (throw "Illegal src/dst !" (list s d)))
				(t (find s d))))
		(j
			(cond
				(i (throw "Illegal src/dst !" (list s d)))
				(t (find d s))))
		(t (eql s d))))

(defcfun assign-find-later-src (i)
	(defq j (dec (length (defq s *src*))) d (elem i *dst*))
	(while (ne j i)
		(if (assign-match-any d (elem j s))
			(setq i j)
			(setq j (dec j)))) j)

(defcfun assign-rotate (i j)
	(setq *src* (cat (slice 0 i (defq s *src*)) (slice j (inc j) s)
				(slice i j s) (slice (inc j) (length s) s))
		*dst* (cat (slice 0 i (defq s *dst*)) (slice j (inc j) s)
				(slice i j s) (slice (inc j) (length s) s))))

(defcfun assign-topology-sort ()
	(defq i 0 c 1000 l (length *dst*))
	(while (and (ne i l) (ne c 0))
		(if (gt (defq j (assign-find-later-src i)) i)
			(progn
				(assign-rotate i j)
				(setq c (dec c)))
			(setq i (inc i))))
	(when (ne i l)
		(throw "Copy cycle detected !" (list *src* *dst*))))

(defcfun assign-asm-to-asm (*src* *dst*)
	(when (ne (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-asm)
	(when (ne 0 (length *dst*))
		(assign-topology-sort)
		(each (lambda (x y)
			(case (vp-type x)
				(r (case (vp-type y)
						(r (vp-cpy-rr x y))
						(ii (vp-cpy-ri-i x (elem 0 y) (elem 1 y)))
						(i (vp-cpy-ri x (elem 0 y) (elem 1 y)))
						(ib (vp-cpy-ri-b x (elem 0 y) (elem 1 y)))
						(is (vp-cpy-ri-s x (elem 0 y) (elem 1 y)))
						(d (vp-cpy-rd x (elem 0 y) (elem 1 y)))
						(otherwise (throw "Invalid dst parameter !" (vp-type y)))))
				(c (vp-cpy-cr x y))
				(i (vp-cpy-ir (elem 0 x) (elem 1 x) y))
				(ii (vp-cpy-ir-i (elem 0 x) (elem 1 x) y))
				(@ (fn-bind (elem 1 x) y))
				(s (fn-string x y))
				($ (vp-lea-p (elem 1 x) y))
				(iui (vp-cpy-ir-ui (elem 0 x) (elem 1 x) y))
				(&i (vp-lea-i (elem 1 x) (elem 2 x) y))
				(d (vp-cpy-dr (elem 0 x) (elem 1 x) y))
				(&d (vp-lea-d (elem 1 x) (elem 2 x) y))
				(iub (vp-cpy-ir-ub (elem 0 x) (elem 1 x) y))
				(ius (vp-cpy-ir-us (elem 0 x) (elem 1 x) y))
				(is (vp-cpy-ir-s (elem 0 x) (elem 1 x) y))
				(ib (vp-cpy-ir-b (elem 0 x) (elem 1 x) y))
				(otherwsie (throw "Invalid src parameter !" (vp-type x))))) *src* *dst*)))

;;;;;;;;;;
; tokenize
;;;;;;;;;;

(defmacro is_white_space_char (_)
	`(not (lt (ascii-code " ") (code ,_) 127)))

(defmacro is_symbol_char (_)
	(list 'find _ "ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"))

(defmacro is_label_char (_)
	(list 'find _ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abcdefghijklmnopqrstuvwxyz"))

(defmacro is_numeral_char (_)
	(list 'find _ "0123456789"))

(defmacro is_allowed_number_char (_)
	(list 'find _ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789"))

(defmacro token-push (k c)
	`(push tokens (list ,k ,c)))

(defcfun token-parse (l)
	(defq m -1 u 1 i 0 p 0 tokens (list))
	(while (lt i (length l))
		(defq c (elem i l)
			s (find c {@$"-*~!+.^)(][</%&|=>}) ;"
			s (if s (add s 2) 0))
		(cond
			((eq m 1)
				;symbol mode
				(if (is_symbol_char c)
					(setq c nil)
					(progn
						(if (find (sym (defq s (slice p i l)))
								'(byte ubyte short ushort int uint long ulong ptr))
							(token-push s 5)
							(token-push s m))
						(setq p i u 0 m -1))))
			((eq m -1)
				;scanning
				(if (is_white_space_char c)
					(setq p (inc i))
					(cond
						((find c ")(][")
							;)(][
							(token-push c s)
							(setq p (inc i) u (logand (sub s 12) 1)))
						((ge s 5)
							;-*~!+.?^</%&|=>
							(setq m s))
						((eq s 0)
							;not whitespace
							(setq m (if (is_numeral_char c) 0 1)))
						(t
							;@$?"
							(setq p (inc i) m s))))
				(setq c nil))
			((ge m 5)
				;op mode
				(cond
					((or (is_white_space_char c) (lt s 16))
						;white space, )([]
						(cond
							((eq u 1)
								;unary operator
								(if (setq s (find (slice p i l) "-*&~!"))
									(token-push (elem s "_#:~!") m)
									(throw "Unknown unary operator !" (slice p i l))))
							(t
								;binary operator
								(token-push (slice p i l) m)
								(setq u 1)))
						(setq p i m -1))
					(t
						(setq c nil))))
			((eq m 4)
				;string mode
				(if (ne s 4)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p (inc i) u 0 m -1))))
			((eq m 2)
				;bind
				(if (is_symbol_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			((eq m 3)
				;label
				(if (is_label_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			((eq m 0)
				;number mode
				(if (is_allowed_number_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			(t (throw "Unknown parse mode !" m)))
		(unless c (setq i (inc i))))
	(unless (eq p i)
		(token-push (slice p i l) m)) tokens)

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

(defmacro rpn-push (s v)
	`(push rpn (list (sym ,s) ,v)))

(defcfun rpn-parse (tokens)
	(defq o (list) rpn (list))
	(each (lambda ((k c))
		(cond
			((ge c 5) ;operator
				(cond
					((find k "([")
						(push o k))
					((eql k ")")
						(while (and (not (eql "(" (setq k (pop o)))) k)
							(rpn-push k -1)))
					((eql k "]")
						(while (and (not (eql "[" (setq k (pop o)))) k)
							(rpn-push k -1))
						(rpn-push "[]" -1))
					(t  ;precedence
						(unless (defq s (get-sym k))
							(throw "Operator not defined !" k))
						(defq v (sub (elem 2 s) (elem 3 s)) x t)
						(while (and x (ne 0 (length o)))
							(setq s (get-sym (elem -2 o)))
							(if (ge v (elem 2 s))
								(rpn-push (pop o) -1)
								(setq x nil)))
						(push o k))))
			(t ;number, symbol, string, symbol, bind, label
				(rpn-push k c)))) tokens)
	(while (defq _ (pop o))
		(rpn-push _ -1)) rpn)

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

(defcvar '*inst* (list) '*vreg* '(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14 _v15))
(each (lambda (_) (defcvar _ nil)) *vreg*)

(defmacro vreg-sym (_)
	(list 'elem _ '*vreg*))

(defmacro add-inst (&rest b)
	(cat '(push *inst*) b))

(defmacro set-type (_)
	(list 'elem-set -2 '*vregt* _))

(defmacro get-type ()
	'(elem -2 *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defmacro reset-reg-stack (_)
	`(defq *vreg* *vreg* *inst* (push (clear *inst*) progn)
		*vregt* (slice 0 ,_ '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))))

(defcfun set-reg-map (l &optional _)
	(setd _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(each! nil nil nil (lambda (_ r)
		(set (env) _ r)) (list *vreg* (if (ne 0 (length l)) (merge l _) _))))

(defcfun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defcfun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defcfun pop-value ()
	(compile-deref?)
	(pop *vregt*)
	(vreg-sym (length *vregt*)))

(defcfun top-value ()
	(when (get-type)
		(compile-deref)
		(set-type nil))
	(top-reg))

(defcfun compile-null ()
	(throw "Null operator !" nil))

(defcfun compile-const (_)
	(add-inst (list 'vp-cpy-cr _ (push-reg nil))))

(defcfun compile-arrow (&optional _)
	(bind '((y w) x) (list (pop-reg) (pop-value)))
	(if _ (push-reg nil))
	(setq w (elem (find w "bBsSiIlLp") '(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
										vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri vp-cpy-ri)))
	(add-inst (list w x y 0)))

(defcfun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (slice 1 -1 w) z (if (eql z "") nil z) w (elem 0 w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem (find w "bBsSiIlLp") '(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
										vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)))
	(add-inst (list w x 0 x)))

(defcfun compile-deref? ()
	(if (get-type)
		(compile-deref)))

(defcfun compile-ref (_)
	(cond
		((not (defq s (get-sym _)))
			;not in symbol table so figure out what it is
			(cond
				((def? (type-sym _))
					;field/member
					(add-inst (list 'vp-cpy-cr _ (push-reg (eval (type-sym _))))))
				((def? _)
					;equate
					(compile-const _))
				(t (throw "Symbol not defined !" _))))
		((eql 'var (elem 1 s))
			;variable
			(add-inst (list 'vp-lea-i rsp (add (get-scope (elem 0 s)) (elem 2 s))
									(push-reg (elem 3 s)))))
		(t (throw "Symbol not a variable !" _))))

(defcfun compile-cast (_)
	(if (defq c (find (sym _) '(byte ubyte short ushort int uint long ulong ptr)))
		(set-type (elem c "bBsSiIlLp"))
		(throw "Unknown type cast" _)))

(defcfun compile-member (_)
	(bind '(_ w) (pop-reg))
	(compile-deref)
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defcfun compile-uaddrof (_)
	(set-type nil))

(defcfun compile-field (_)
	(bind '(_ w) (pop-reg))
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defcfun compile-index (_)
	(defq _ (pop-value))
	(compile-deref)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defcfun compile-uminus (_)
	(add-inst (list 'vp-mul-cr -1 (top-value))))

(defcfun compile-uderef (_)
	(compile-deref))

(defcfun compile-unot (_)
	(add-inst (list 'vp-xor-cr -1 (top-value))))

(defcfun compile-mul (_)
	(add-inst (list 'vp-mul-rr (pop-value) (top-value))))

(defcfun compile-fmul (_)
	(add-inst
		(list 'vp-mul-rr (pop-value) (defq _ (top-value)))
		(list 'vp-asr-cr 16 _)))

(defcfun compile-divu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)))

(defcfun compile-remu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)
		(list 'vp-cpy-rr _ x)))

(defcfun compile-div (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defcfun compile-fdiv (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-shl-cr 16 x)
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defcfun compile-rem (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)
		(list 'vp-cpy-rr _ x)))

(defcfun compile-plus (_)
	(add-inst (list 'vp-add-rr (pop-value) (top-value))))

(defcfun compile-minus (_)
	(add-inst (list 'vp-sub-rr (pop-value) (top-value))))

(defcfun compile-lshift (_)
	(add-inst (list 'vp-shl-rr (pop-value) (top-value))))

(defcfun compile-rshift (_)
	(add-inst (list 'vp-shr-rr (pop-value) (top-value))))

(defcfun compile-arshift (_)
	(add-inst (list 'vp-asr-rr (pop-value) (top-value))))

(defcfun compile-cmp (_)
	(add-inst (list (elem (find _ '(== != <= >= > <))
		'(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr))
		(pop-value) (top-value))))

(defcfun compile-and (_)
	(add-inst (list 'vp-and-rr (pop-value) (top-value))))

(defcfun compile-xor (_)
	(add-inst (list 'vp-xor-rr (pop-value) (top-value))))

(defcfun compile-or (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defcfun compile-ulnot (_)
	(add-inst (list 'vp-lnot-rr (tmp-reg) (top-value))))

(defcfun compile-land (_)
	(add-inst (list 'vp-land-rr (pop-value) (top-value))))

(defcfun compile-lor (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defcfun compile-operator (_)
	(cond
		((not (defq s (get-sym _)))
			(throw "Operator not defined !" _))
		((not (eql 'op (elem 1 s)))
			(throw "Not an operator !" _))
		(t
			((elem 4 s) _))))

(defcfun compile-string (_)
	(add-inst (list 'fn-string _ (push-reg nil))))

(defcfun compile-bind (_)
	(add-inst `(fn-bind ',_ ,(push-reg nil))))

(defcfun compile-label (_)
	(add-inst `(vp-lea-p ',_ ,(push-reg nil))))

(defcfun compile-rpn-list (rpn)
	(each (lambda ((k c))
		(if (le -1 c 4)
			(eval (elem (inc c)
				'((compile-operator k)
				(compile-const (to-num k))
				(compile-ref k)
				(compile-bind k)
				(compile-label k)
				(compile-string (str k)))))
			(throw "Unknown token type !" k))) rpn))

(new-scope)
(operator "ptr" 0 1 compile-cast)
(operator "byte" 0 1 compile-cast)
(operator "ubyte" 0 1 compile-cast)
(operator "short" 0 1 compile-cast)
(operator "ushort" 0 1 compile-cast)
(operator "int" 0 1 compile-cast)
(operator "uint" 0 1 compile-cast)
(operator "long" 0 1 compile-cast)
(operator "ulong" 0 1 compile-cast)
(operator "." 1 0 compile-field)
(operator "->" 1 0 compile-member)
(operator "[]" 1 0 compile-index)
(operator ":" 2 1 compile-uaddrof)
(operator "_" 2 1 compile-uminus)
(operator "#" 2 1 compile-uderef)
(operator "~" 2 1 compile-unot)
(operator "!" 2 1 compile-ulnot)
(operator "*>" 3 0 compile-fmul)
(operator "</" 3 0 compile-fdiv)
(operator "*" 3 0 compile-mul)
(operator "/" 3 0 compile-divu)
(operator "%" 3 0 compile-remu)
(operator "//" 3 0 compile-div)
(operator "%%" 3 0 compile-rem)
(operator "+" 4 0 compile-plus)
(operator "-" 4 0 compile-minus)
(operator "<<" 5 0 compile-lshift)
(operator ">>" 5 0 compile-rshift)
(operator ">>>" 5 0 compile-arshift)
(operator "<" 6 0 compile-cmp)
(operator ">" 6 0 compile-cmp)
(operator "<=" 6 0 compile-cmp)
(operator ">=" 6 0 compile-cmp)
(operator "==" 7 0 compile-cmp)
(operator "!=" 7 0 compile-cmp)
(operator "&" 8 0 compile-and)
(operator "^" 9 0 compile-xor)
(operator "|" 10 0 compile-or)
(operator "&&" 11 0 compile-land)
(operator "||" 12 0 compile-lor)
(operator "=>" 13 0 compile-arrow)
(operator "(" 14)
(operator ")" 14)
(operator "[" 14)
(operator "]" 14)
(new-scope)

;;;;;;;;;;;
; optimizer
;;;;;;;;;;;

(defmacro opt-sxx-cr-ops ()
	''(vp-seq-cr vp-sne-cr vp-sle-cr vp-sge-cr vp-sgt-cr vp-slt-cr))

(defmacro opt-sxx-rr-ops ()
	''(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr))

(defmacro opt-read-ops ()
	''(vp-cpy-ir vp-cpy-ir-b vp-cpy-ir-s vp-cpy-ir-i vp-cpy-ir-ub vp-cpy-ir-us vp-cpy-ir-ui))

(defmacro opt-write-ops ()
	''(vp-cpy-ri vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-i))

(defmacro opt-rr-ops ()
	''(vp-add-rr vp-sub-rr vp-mul-rr vp-and-rr vp-or-rr vp-xor-rr vp-shl-rr vp-shr-rr vp-asr-rr))

(defmacro opt-cr-ops ()
	''(vp-add-cr vp-sub-cr vp-mul-cr vp-and-cr vp-or-cr vp-xor-cr vp-shl-cr vp-shr-cr vp-asr-cr))

(defmacro opt-fold-cpy-ops ()
	''(add sub mul logand logior logxor shl shr asr))

(defmacro opt-fold-cr-ops ()
	''(add add mul logand logior logxor add add add))

(defmacro opt-cr-fuse-ops ()
	''(vp-add-cr vp-sub-cr))

(defmacro opt-uses (r _)
	`(find ,r ,_))

(defcfun opt-find-1 (_ r x)
	(setq _ (some! _ 0 t (lambda (e)
				(cond
					((match? e x) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (ne _ -1) _)))

(defcfun opt-find-2 (_ r x y)
	(setq _ (some! _ 0 t (lambda (e)
				(cond
					((match? e x) (setq m 0) _)
					((match? e y) (setq m 1) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (ne _ -1) _)))

(defcfun opt-inst-list ()
	(defq i 0 p nil m nil)
	(while (lt (setq i (inc i)) (length *inst*))
		(cond
			;variable loading and offset folding
			((find (defq e (elem i *inst*) o (elem 0 e)) (opt-read-ops))
				(when (opt-find-2 i (defq r (elem 1 e)) (list 'vp-lea-i rsp '_ r) (list 'vp-add-cr '_ r))
					(cond
						((eq m 0)
							(elem-set 1 e rsp)
							(elem-set 2 e (list 'add (elem 2 (elem p *inst*)) (elem 2 e))))
						((eq m 1)
							(elem-set 2 e (list 'add (elem 1 (elem p *inst*)) (elem 2 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;variable writing and offset folding
			((find o (opt-write-ops))
				(when (opt-find-2 i (defq r (elem 2 e)) (list 'vp-lea-i rsp '_ r) (list 'vp-add-cr '_ r))
					(cond
						((eq m 0)
							(elem-set 2 e rsp)
							(elem-set 3 e (list 'add (elem 2 (elem p *inst*)) (elem 3 e))))
						((eq m 1)
							(elem-set 3 e (list 'add (elem 1 (elem p *inst*)) (elem 3 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (elem 1 e)))))
				(elem-set 0 e 'vp-shl-cr)
				(elem-set 1 e s)
				(setq i (dec i)))
			;constant propagation
			((defq c (find o (opt-rr-ops)))
				(when (opt-find-1 i (defq r (elem 1 e)) (list 'vp-cpy-cr '_ r))
					(elem-set 0 e (elem c (opt-cr-ops)))
					(elem-set 1 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* (lambda))
					(setq i (dec i))))
			;arithmetic reassignment and constant folding
			((and (defq c (find o (opt-cr-ops)))
				(opt-find-2 i (defq r (elem 2 e)) (list 'vp-cpy-cr '_ r) (list o '_ r)))
					(cond
						((eq m 0)
							(setq c (elem c (opt-fold-cpy-ops)))
							(elem-set 0 e 'vp-cpy-cr))
						((eq m 1)
							(setq c (elem c (opt-fold-cr-ops)))))
					(elem-set 1 e (list c (elem 1 (elem p *inst*)) (elem 1 e)))
					(elem-set p *inst* (lambda)))
			;constant fusion
			((and (defq c (find o (opt-cr-fuse-ops)))
				(opt-find-2 i (defq r (elem 2 e)) (list 'vp-add-cr '_ r) (list 'vp-sub-cr '_ r)))
					(cond
						((eq m 0)
							(setq c (elem c '(add sub))))
						((eq m 1)
							(setq c (elem c '(sub add)))))
					(elem-set 1 e (list c (elem 1 e) (elem 1 (elem p *inst*))))
					(elem-set p *inst* (lambda)))
			;compare constant forwarding
			((and (defq c (find o (opt-sxx-rr-ops)))
				(opt-find-1 i (defq r (elem 1 e)) (list 'vp-cpy-cr '_ r))
				(le -0x80000000 (defq v (eval (elem 1 (elem p *inst*)))) 0x7fffffff))
					(elem-set 0 e (elem c (opt-sxx-cr-ops)))
					(elem-set 1 e v)
					(elem-set p *inst* (lambda)))
			)))

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

(defcfun print-inst (_)
	(and (lst? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defcfun assign-asm-to-script (*src* *dst* _)
	(unless (eq (length *src*) (length (setq *dst* (map trim (split *dst* ",")))))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-script)
	(when (ne 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(each! nil nil nil (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-arrow)) (list *dst*))
		(when debug_inst
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map (reduce-rev (lambda (x y) (push x y)) *src* (list)) _)
		(eval *inst* *compile-env*)))

(defcfun assign-script-to-asm (*src* *dst* _)
	(unless (eq (length (setq *src* (split *src* ","))) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(when (ne 0 (length *dst*))
		(reset-reg-stack 0)
		(each! nil nil nil (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-deref?)) (list *src*))
		(when debug_inst
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map (cat *dst*) _)
		(eval *inst* *compile-env*)))

(defcfun assign-script-to-script (*src* *dst* _)
	(setq *src* (split *src* ",") *dst* (map trim (split *dst* ",")))
	(unless (eq (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-script)
	(when (ne 0 (length *dst*))
		(reset-reg-stack 0)
		(each! nil nil nil (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))) (list *src*))
		(each! -1 0 nil (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-arrow)) (list *dst*))
		(when debug_inst
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map '() _)
		(eval *inst* *compile-env*)))

(defcfun assign (&optional _1 _2 _3)
	;optional src, dst, compiler regs
	(cond
		((and (defq d (str? (setq _2 (opt _2 '()))) s (str? (setq _1 (opt _1 '())))) (not d))
			(assign-script-to-asm _1 _2 _3))
		((and (not s) (not d))
			(assign-asm-to-asm _1 _2))
		((and (not s) d)
			(assign-asm-to-script _1 _2 _3))
		(t (assign-script-to-script _1 _2 _3))))
