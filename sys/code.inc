;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

(defcvar '*switch* nil '*switch-nxt* nil '*switch-stk* (list))

(defmacro beg-sym ()
	'(sym-cat "_beg_" (str *switch*)))

(defmacro end-sym ()
	'(sym-cat "_end_" (str *switch*)))

(defmacro loc-sym (n)
	`(sym-cat "_loc_" (str *switch*) "_" (str ,n)))

(defmacro cnt-sym ()
	'(sym-cat "_cnt_" (str *switch*)))

(defcfun goto (l)
	(vp-jmp l))

(defcfun gotoif (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 != 0)))
	(bind '(d o s) e)
	(defq c (type s))
	(cond
		((eql c 'c)
			(cond
				((eql o '==) (vp-beq-cr s d l))
				((eql o '!=) (vp-bne-cr s d l))
				((eql o '<) (vp-blt-cr s d l))
				((eql o '>) (vp-bgt-cr s d l))
				((eql o '<=) (vp-ble-cr s d l))
				((eql o '>=) (vp-bge-cr s d l))
				(t (throw "No such comparator" o))))
		((eql c 'r)
			(cond
				((eql o '==) (vp-beq-rr s d l))
				((eql o '!=) (vp-bne-rr s d l))
				((eql o '<) (vp-blt-rr s d l))
				((eql o '>) (vp-bgt-rr s d l))
				((eql o '<=) (vp-ble-rr s d l))
				((eql o '>=) (vp-bge-rr s d l))
				(t (throw "No such comparator" o))))
		(t (throw "No such compare mode" c))))

(defcfun gotoifnot (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 != 0)))
	(bind '(d o s) e)
	(defq c (type s))
	(cond
		((eql c 'c)
			(cond
				((eql o '==) (vp-bne-cr s d l))
				((eql o '!=) (vp-beq-cr s d l))
				((eql o '<) (vp-bge-cr s d l))
				((eql o '>) (vp-ble-cr s d l))
				((eql o '<=) (vp-bgt-cr s d l))
				((eql o '>=) (vp-blt-cr s d l))
				(t (throw "No such comparator" o))))
		((eql c 'r)
			(cond
				((eql o '==) (vp-bne-rr s d l))
				((eql o '!=) (vp-beq-rr s d l))
				((eql o '<) (vp-bge-rr s d l))
				((eql o '>) (vp-ble-rr s d l))
				((eql o '<=) (vp-bgt-rr s d l))
				((eql o '>=) (vp-blt-rr s d l))
				(t (throw "No such comparator" o))))
		(t (throw "No such compare mode" c))))

(defcmacro errorif (e l)
	(if debug_mode
		`(gotoif ,e ,l)))

(defcmacro errorifnot (e l)
	(if debug_mode
		`(gotoifnot ,e ,l)))

(defcmacro errorassign (e d)
	(unless debug_mode
		(setq e '(list 1)))
	`(assign ,e ,d))

(defcmacro errorcases (&rest e)
	(if debug_mode
		`(progn ~e)))

(defcfun switch ()
	(push *switch-stk* *switch*)
	(setq *switch* *switch-nxt* *switch-nxt* (inc *switch-nxt*))
	(defcvar (cnt-sym) 0))

(defcfun default ()
	(vp-label (loc-sym (defq s (cnt-sym) c (eval s))))
	(set s (inc c)))

(defcfun endswitch ()
	(vp-label (end-sym))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcfun case (e)
	(default)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defcfun casenot (e)
	(default)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defcfun break (&optional _)
	(goto (if _ _ (end-sym))))

(defcfun breakif (e &optional _)
	(gotoif e (if _ _ (end-sym))))

(defcfun breakifnot (e &optional _)
	(gotoifnot e (if _ _ (end-sym))))

(defcfun continue ()
	(goto (beg-sym)))

(defcfun continueif (e)
	(gotoif e (beg-sym)))

(defcfun continueifnot (e)
	(gotoifnot e (beg-sym)))

(defcfun loop-start (&optional _)
	(switch)
	(vp-label (beg-sym))
	(if _ (defcvar _ (end-sym))))

(defcfun loop-while (e)
	(switch)
	(vp-label (beg-sym))
	(breakifnot e))

(defcfun loop-whilenot (e)
	(switch)
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

(defcfun vpif (e)
	(switch)
	(gotoifnot e (loc-sym (eval (cnt-sym)))))

(defcfun vpifnot (e)
	(switch)
	(gotoif e (loc-sym (eval (cnt-sym)))))

(defcfun elseif (e)
	(break)
	(case e))

(defcfun elseifnot (e)
	(break)
	(casenot e))

(defcfun else ()
	(break)
	(default))

(defcfun endif ()
	(endswitch))

;;;;;;;
; enums
;;;;;;;

(defcvar '*enum* nil '*enum-offset* nil)

(defcfun def-enum (_1 &optional _2)
	(setq *enum* _1 *enum-offset* (opt _2 0)))

(defcfun def-enum-end ()
	(setq *enum* nil *enum-offset* nil))

(defcfun enum (&rest _)
	(each (lambda (_)
		(setq _ (sym-cat *enum* "_" _))
		(if (def? _)
			(throw "Enum redefined !" _))
		(defcvar _ *enum-offset*)
		(setq *enum-offset* (inc *enum-offset*))) _))

;;;;;;
; bits
;;;;;;

(defcvar '*bit* nil '*bit-offset* nil)

(defcfun def-bit (_1 &optional _2)
	(setq *bit* _1 *bit-offset* (opt _2 0)))

(defcfun def-bit-end ()
	(setq *bit* nil *bit-offset* nil))

(defcfun bit (&rest _)
	(each (lambda (_)
		(setq _ (sym-cat *bit* "_" _))
		(if (def? _)
			(throw "Bit redefined !" _))
		(defcvar _ (bit-shl 1 *bit-offset*))
		(setq *bit-offset* (inc *bit-offset*))) _))

;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

(defcvar '*sym-sp* 0 '*syms* (list (list 0 (list) (list))))

(defcfun get-scope (_)
	(defq o 0)
	(each! (inc _) -2 nil (lambda (_)
			(setq o (add o (elem 0 _)))) (list *syms*)) o)

(defcfun push-scope ()
	(local-align)
	(elem-set 0 (defq _ (pop *syms*)) *sym-sp*)
	(push *syms* _ (list 0 (list) (list)))
	(when (ne *sym-sp* 0)
		(when debug_inst
			(print "(vp-sub-cr " *sym-sp* " r4)"))
		(vp-sub-cr *sym-sp* r4)
		(setq *sym-sp* 0)))

(defcfun pop-scope-syms ()
	(pop *syms*)
	(defq _ (elem 0 (pop *syms*)))
	(push *syms* (list 0 (list) (list)))
	(setq *sym-sp* 0) _)

(defcfun pop-scope ()
	(defq _ (pop-scope-syms))
	(when (ne _ 0)
		(when debug_inst
			(print "(vp-add-cr " _ " r4)"))
		(vp-add-cr _ r4)))

(defcfun pop-scope-checked ()
	(defq _ (pop-scope-syms))
	(when (ne (length *syms*) 2)
		(throw "Unbalanced scopes !" _)))

(defcfun unwind ()
	(when (ne (defq _ (get-scope -1)) 0)
		(when debug_inst
			(print "(vp-add-cr " _ " r4)"))
		(vp-add-cr _ r4)))

(defcfun return ()
	(unwind)
	(when debug_inst
		(print "(vp-ret)"))
	(vp-ret))

(defcfun def-sym (_1 _2 &rest _3)
	;_1 name
	;_2 type
	;_3 values
	(setq _1 (sym _1))
	(if (find _1 (elem 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" _1))
	(push (elem 1 _) _1)
	(push (elem 2 _) (cat (list (length *syms*) _2) _3))
	(push *syms* _))

(defcfun get-sym (_1)
	;_1 name
	(setq _1 (sym _1))
	(defq _ (length *syms*))
	(while (and (ge (setq _ (dec _)) 0)
				(not (defq s (find _1 (elem 1 (elem _ *syms*)))))))
	(if (ge _ 0) (elem s (elem 2 (elem _ *syms*))) nil))

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
	`(sym-cat "_t_" (str ,_)))

(defmacro struct-sym (_)
	`(sym-cat *struct* "_" ,_))

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
	(setq *struct* s *struct-sp* (eval (sym-cat (opt o "null") "_size"))))

(defcfun def-struct-end ()
	(defcvar (sym-cat *struct* "_size") *struct-sp*)
	(setq *struct* nil *struct-sp* nil))

(defmacro def-type (n s y)
	`(defcfun ,n (&rest f)
		(each (lambda (_)
			(local-align ,s)
			(cond
				(*struct*
					(setq _ (check-field _))
					(defcvar _ *struct-sp* (type-sym _) ,y)
					(setq *struct-sp* (add *struct-sp* ,s)))
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
			(setq _ (check-field _))
			(defcvar _ *struct-sp*))
		(t
			(throw "Attempt to define offset into local stack !" _))))

(defcfun struct (_ s)
	(if (sym? s) (setq s (eval (sym-cat s "_size"))))
	(cond
		(*struct*
			(setq _ (check-field _))
			(defcvar _ *struct-sp* (type-sym _) nil)
			(setq *struct-sp* (add *struct-sp* s)))
		(t
			(def-sym _ 'var *sym-sp* 0 "")
			(setq *sym-sp* (add *sym-sp* s)))))

(defcfun union (&rest _)
	(if (notevery lst? _)
		(throw "Union needs lists as paramaters !" _))
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

(defcvar '*src* nil '*dst* nil)

(defcfun map-remove-ignored ()
	(defq s (list) d (list))
	(each (lambda (x y) (unless (or (eql x y) (eql y '_) (eql y "_")) (push s x) (push d y))) *src* *dst*)
	(setq *src* s *dst* d))

(defcfun match-any (d s)
	(defq i (lst? d) j (lst? s))
	(cond
		(i (cond
			(j (throw "Illegal src/dst !" (list s d)))
			(t (find s d))))
		(j (cond
			(i (throw "Illegal src/dst !" (list s d)))
			(t (find d s))))
		(t (eql s d))))

(defcfun find-later-src (i)
	(defq j (dec (length (defq s *src*))) d (elem i *dst*))
	(while (ne j i)
		(if (match-any d (elem j s))
			(setq i j)
			(setq j (dec j)))) j)

(defcfun map-rotate (i j)
	(setq *src* (cat (slice 0 i (defq s *src*)) (slice j (inc j) s)
				(slice i j s) (slice (inc j) (length s) s))
		*dst* (cat (slice 0 i (defq s *dst*)) (slice j (inc j) s)
				(slice i j s) (slice (inc j) (length s) s))))

(defcfun map-topology-sort ()
	(defq i 0 c 1000 l (length *dst*))
	(while (and (ne i l) (ne c 0))
		(defq j (find-later-src i))
		(if (gt j i)
			(progn
				(map-rotate i j)
				(setq c (dec c)))
			(setq i (inc i))))
	(when (ne i l)
		(throw "Copy cycle detected !" (list *src* *dst*))))

(defcfun map-src-to-dst ()
	(when (ne (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(map-topology-sort)
		(each (lambda (x y)
			(defq i (type x))
			(cond
				((eql i 'r)
					(setq i (type y))
					(cond
						((eql i 'r) (vp-cpy-rr x y))
						((eql i 'i) (vp-cpy-ri x (elem 0 y) (elem 1 y)))
						((eql i 'ib) (vp-cpy-ri-b x (elem 0 y) (elem 1 y)))
						((eql i 'is) (vp-cpy-ri-s x (elem 0 y) (elem 1 y)))
						((eql i 'ii) (vp-cpy-ri-i x (elem 0 y) (elem 1 y)))
						((eql i 'd) (vp-cpy-rd x (elem 0 y) (elem 1 y)))
						(t (throw "Invalid dst parameter !" i))))
				((eql i 'c) (vp-cpy-cr x y))
				((eql i 'i) (vp-cpy-ir (elem 0 x) (elem 1 x) y))
				((eql i 'ib) (vp-cpy-ir-b (elem 0 x) (elem 1 x) y))
				((eql i 'iub) (vp-cpy-ir-ub (elem 0 x) (elem 1 x) y))
				((eql i 'is) (vp-cpy-ir-s (elem 0 x) (elem 1 x) y))
				((eql i 'ius) (vp-cpy-ir-us (elem 0 x) (elem 1 x) y))
				((eql i 'ii) (vp-cpy-ir-i (elem 0 x) (elem 1 x) y))
				((eql i 'iui) (vp-cpy-ir-ui (elem 0 x) (elem 1 x) y))
				((eql i 'd) (vp-cpy-dr (elem 0 x) (elem 1 x) y))
				((eql i '&i) (vp-lea-i (elem 1 x) (elem 2 x) y))
				((eql i '&d) (vp-lea-d (elem 1 x) (elem 2 x) y))
				((eql i 's) (fn-string x y))
				((eql i '@) (fn-bind (elem 1 x) y))
				((eql i '$) (vp-lea-p (elem 1 x) y))
				(t (throw "Invalid src parameter !" i)))) *src* *dst*)))

;;;;;;;;;;
; tokenize
;;;;;;;;;;

(defcvar '*token* (list))

(defmacro push-token (&rest b)
	`(push *token* (list ~b)))

(defmacro is_white_space_char (_)
	`(not (lt (ascii " ") (code ,_) 127)))

(defmacro is_symbol_char (_)
	`(find ,_ "ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"))

(defmacro is_label_char (_)
	`(find ,_ "0123456789_abcdefghijklmnopqrstuvwxyz"))

(defmacro is_numeral_char (_)
	`(find ,_ "0123456789"))

(defmacro is_allowed_number_char (_)
	`(find ,_ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789"))

(defcfun set-token-list (l)
	(clear *token*)
	(defq m -1 u 1 i 0 p 0)
	(while (lt i (length l))
		(defq c (elem i l)
			s (find c {@$"-*~!+.^)(][</%&|=>}) ;"
			s (if s (add s 2) 0))
		(cond
			((eq m -1)
				;scanning
				(if (is_white_space_char c)
					(setq p (inc i))
					(cond
						((find c ")(][")
							;)(][
							(push-token c s)
							(setq p (inc i) u (bit-and (sub s 12) 1)))
						((ge s 5)
							;-*~!+.?^</%&|=>
							(setq m s))
						((eq s 0)
							;not whitespace
							(setq m (if (is_numeral_char c) 0 1)))
						(t
							;@$"
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
									(push-token (elem s "_#:~!") m)
									(throw "Unknown unary operator !" (slice p i l))))
							(t
								;binary operator
								(push-token (slice p i l) m)
								(setq u 1)))
						(setq p i m -1))
					(t
						(setq c nil))))
			((eq m 0)
				;number mode
				(if (is_allowed_number_char c)
					(setq c nil)
					(progn
						(push-token (slice p i l) m)
						(setq p i u 0 m -1))))
			((eq m 1)
				;symbol mode
				(if (is_symbol_char c)
					(setq c nil)
					(progn
						(if (find (sym (defq s (slice p i l)))
								'(byte ubyte short ushort int uint long ulong ptr))
							(push-token s 5)
							(push-token s m))
						(setq p i u 0 m -1))))
			((eq m 2)
				;bind
				(if (is_symbol_char c)
					(setq c nil)
					(progn
						(push-token (slice p i l) m)
						(setq p i u 0 m -1))))
			((eq m 3)
				;label
				(if (is_label_char c)
					(setq c nil)
					(progn
						(push-token (slice p i l) m)
						(setq p i u 0 m -1))))
			(t
				;string mode
				(if (ne s 4)
					(setq c nil)
					(progn
						(push-token (slice p i l) m)
						(setq p (inc i) u 0 m -1)))))
		(unless c (setq i (inc i))))
	(unless (eq p i)
		(push-token (slice p i l) m)))

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

(defcvar '*rpn* (list))

(defmacro push-rpn (s v)
	`(push *rpn* (list (sym ,s) ,v)))

(defcfun token-to-rpn ()
	(clear *rpn*)
	(defq o (list))
	(each (lambda ((k c))
		(cond
			((ge c 5) ;operator
				(cond
					((find k "([")
						(push o k))
					((eql k ")")
						(while (and (not (eql "(" (setq k (pop o)))) k)
							(push-rpn k -1)))
					((eql k "]")
						(while (and (not (eql "[" (setq k (pop o)))) k)
							(push-rpn k -1))
						(push-rpn "[]" -1))
					(t ;precedence
						(unless (defq s (get-sym k))
							(throw "Operator not defined !" k))
						(defq v (sub (elem 2 s) (elem 3 s)) x t)
						(while (and x (ne 0 (length o)))
							(setq s (get-sym (elem -2 o)))
							(if (ge v (elem 2 s))
								(push-rpn (pop o) -1)
								(setq x nil)))
						(push o k))))
			(t ;number, symbol, string, symbol, bind, label
				(push-rpn k c)))) *token*)
	(while (defq _ (pop o))
		(push-rpn _ -1)))

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

(defcvar '*s* nil '*r* nil '*inst* (list)
	'*vreg* '(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14)
	'*vregt* '(_t0 _t1 _t2 _t3 _t4 _t5 _t6 _t7 _t8 _t9 _t10 _t11 _t12 _t13 _t14))
(defmacro vreg-sym (_) `(elem ,_ *vreg*))
(defmacro vreg-type-sym (_) `(elem ,_ *vregt*))
(each (lambda (_) (defcvar (vreg-sym _) nil (vreg-type-sym _) nil)) (range 0 15))

(defmacro inc-stack ()
	'(setq *s* (inc *s*)))

(defmacro dec-stack ()
	'(setq *s* (dec *s*)))

(defmacro nil-type ()
	'(set (vreg-type-sym *s*) nil))

(defmacro add-inst (&rest b)
	`(push *inst* ~b))

(defcfun reset-reg-stack (&optional _1)
	;_1 = reserved slots
	(setd _1 0)
	(setq *s* _1 *r* 0)
	(each! nil _1 nil (lambda (_)
		(set _ nil)) (list (slice 0 _1 *vregt*))))

(defcfun res-reg-def (_1)
	;_1 = 'rX'
	(set (vreg-sym *r*) _1 (vreg-type-sym *r*) nil)
	(setq *r* (inc *r*)))

(defcfun add-reg-def (_1)
	;_1 = rX
	(unless (find _1 (map eval (slice 0 *r* *vreg*)))
		(set (vreg-sym *r*) _1)
		(setq *r* (inc *r*))))

(defcfun fill-reg-def (&optional _)
	(setd _ '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
	(each! nil nil nil add-reg-def (list _)))

(defcfun undef-reg-def ()
	(each! nil nil nil (lambda (_1 _2)
		(set _1 nil _2 nil)) (list (slice 0 *r* *vreg*) (slice 0 *r* *vregt*))))

(defcfun pop-type (s)
	(if (defq u (vreg-type-sym s) w (eval u))
		(progn
			(defq v (slice 1 -1 w) v (if (eql v "") nil v))
			(set u v)
			(elem 0 w))
		(throw "No type info !" u)))

(defcfun pop-value ()
	(dec-stack)
	(compile-deref?)
	(nil-type)
	(vreg-sym *s*))

(defcfun compile-null ()
	(throw "Null operator !" nil))

(defcfun compile-const (_1)
	(nil-type)
	(add-inst `(vp-cpy-cr ,_1 ,(vreg-sym *s*))))

(defcfun compile-assign ()
	(defq y (vreg-sym (dec-stack)) w (pop-type *s*) x (pop-value))
	(setq w (elem (find w "bBsSiIlLp") '(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
										vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri vp-cpy-ri)))
	(add-inst `(,w ,x ,y 0)))

(defcfun compile-deref ()
	(defq v (vreg-sym *s*) w (pop-type *s*))
	(setq w (elem (find w "bBsSiIlLp") '(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
										vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)))
	(add-inst `(,w ,v 0 ,v)))

(defcfun compile-num (_1)
	(compile-const (to-num _1)))

(defcfun compile-ref (_1)
	(cond
		((not (defq s (get-sym _1)))
			;not in symbol table so figure out what it is
			(cond
				((eql _1 'RXX)
					;reserved slot
					(nil-type))
				((def? (type-sym _1))
					;field/member
					(set (vreg-type-sym *s*) (eval (type-sym _1)))
					(add-inst `(vp-cpy-cr ,_1 ,(vreg-sym *s*))))
				((def? _1)
					;equate
					(compile-const _1))
				(t (throw "Symbol not defined !" _1))))
		((eql 'var (elem 1 s))
			;variable
			(set (vreg-type-sym *s*) (elem 3 s))
			(setq s (add (get-scope (elem 0 s)) (elem 2 s)))
			(add-inst `(vp-lea-i r4 ,s ,(vreg-sym *s*))))
		(t (throw "Symbol not a variable !" _1))))

(defcfun compile-deref? ()
	(when (eval (vreg-type-sym *s*))
		(compile-deref)))

(defcfun compile-cast (o)
	(if (defq c (find (sym o) '(byte ubyte short ushort int uint long ulong ptr)))
		(set (vreg-type-sym (dec-stack)) (elem c "bBsSiIlLp"))
		(throw "Unknown type cast" o)))

(defcfun compile-member (o)
	(defq s (dec *s*) x (vreg-sym (setq *s* (sub *s* 2))) y (vreg-sym s))
	(compile-deref)
	(add-inst `(vp-add-rr ,y ,x))
	(set (vreg-type-sym *s*) (eval (vreg-type-sym s))))

(defcfun compile-uaddrof (o)
	(set (vreg-type-sym (dec-stack)) nil))

(defcfun compile-field (o)
	(defq s (dec *s*) x (vreg-sym (setq *s* (sub *s* 2))) y (vreg-sym s))
	(add-inst `(vp-add-rr ,y ,x))
	(set (vreg-type-sym *s*) (eval (vreg-type-sym s))))

(defcfun compile-index (o)
	(defq y (pop-value))
	(dec-stack)
	(compile-deref)
	(add-inst `(vp-add-rr ,y ,(vreg-sym *s*))))

(defcfun compile-uminus (o)
	(add-inst `(vp-mul-cr -1 ,(pop-value))))

(defcfun compile-uderef (o)
	(dec-stack)
	(compile-deref))

(defcfun compile-unot (o)
	(add-inst `(vp-xor-cr -1 ,(pop-value))))

(defcfun compile-mul (o)
	(add-inst `(vp-mul-rr ,(pop-value) ,(pop-value))))

(defcfun compile-fmul (o)
	(defq x (pop-value) y (pop-value))
	(add-inst
		`(vp-mul-rr ,x ,y)
		`(vp-asr-cr 16 ,y)))

(defcfun compile-divu (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr-u ,z ,y ,x)))

(defcfun compile-remu (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr-u ,z ,y ,x)
		`(vp-cpy-rr ,y ,x)))

(defcfun compile-div (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr ,z ,y ,x)))

(defcfun compile-fdiv (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-shl-cr 16 ,x)
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr ,z ,y ,x)))

(defcfun compile-rem (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr ,z ,y ,x)
		`(vp-cpy-rr ,y ,x)))

(defcfun compile-plus (o)
	(add-inst `(vp-add-rr ,(pop-value) ,(pop-value))))

(defcfun compile-minus (o)
	(add-inst `(vp-sub-rr ,(pop-value) ,(pop-value))))

(defcfun compile-lshift (o)
	(add-inst `(vp-shl-rr ,(pop-value) ,(pop-value))))

(defcfun compile-rshift (o)
	(add-inst `(vp-shr-rr ,(pop-value) ,(pop-value))))

(defcfun compile-arshift (o)
	(add-inst `(vp-asr-rr ,(pop-value) ,(pop-value))))

(defcfun compile-cmp (o)
	(defq y (pop-value) x (pop-value))
	(add-inst
		`(vpif (list ,x ',(sym o) ,y))
			`(vp-or-cr -1 ,x)
		'(else)
			`(vp-xor-rr ,x ,x)
		'(endif)))

(defcfun compile-and (o)
	(add-inst `(vp-and-rr ,(pop-value) ,(pop-value))))

(defcfun compile-xor (o)
	(add-inst `(vp-xor-rr ,(pop-value) ,(pop-value))))

(defcfun compile-or (o)
	(add-inst `(vp-or-rr ,(pop-value) ,(pop-value))))

(defcfun compile-land (o)
	(defq y (pop-value) x (pop-value))
	(add-inst
		`(vpif (list ,x '!= 0))
			`(vp-xor-rr ,x ,x)
			`(vpif (list ,y '!= 0))
				`(vp-sub-cr 1 ,x)
			'(endif)
		'(endif)))

(defcfun compile-lor (o)
	(add-inst `(vp-or-rr ,(pop-value) ,(pop-value))))

(defcfun compile-ulnot (o)
	(defq x (pop-value))
	(add-inst
		`(vpif (list ,x '== 0))
			`(vp-sub-cr 1 ,x)
		'(else)
			`(vp-xor-rr ,x ,x)
		'(endif)))

(defcfun compile-operator (_1)
	(cond
		((not (defq s (get-sym _1)))
			(throw "Operator not defined !" _1))
		((not (eql 'op (elem 1 s)))
			(throw "Not an operator !" _1))
		(t
			((elem 4 s) _1))))

(defcfun compile-string (_1)
	(nil-type)
	(add-inst `(fn-string ,_1 ,(vreg-sym *s*))))

(defcfun compile-bind (_1)
	(nil-type)
	(add-inst `(fn-bind ',_1 ,(vreg-sym *s*))))

(defcfun compile-label (_1)
	(nil-type)
	(add-inst `(vp-lea-p ',_1 ,(vreg-sym *s*))))

(defcfun compile-rpn-list ()
	(each (lambda ((k c))
		(if (le -1 c 4)
			(eval (elem (inc c)
				'((compile-operator k)
				(compile-num k)
				(compile-ref k)
				(compile-bind k)
				(compile-label k)
				(compile-string (str k)))))
			(throw "Unknown token type !" k))
		(setq *s* (inc *s*))) *rpn*))

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
(operator "(" 14)
(operator ")" 14)
(operator "[" 14)
(operator "]" 14)
(push-scope)

;;;;;;;;;;;
; optimizer
;;;;;;;;;;;

(defmacro opt-find? (i r &rest b)
	;results in p and m of parent env !
	(cond
		((eq 1 (length b))
			`(opt-find-1 ,i ,r ~b))
		((eq 2 (length b))
			`(opt-find-2 ,i ,r ~b))
		(t (throw "Wrong number of args in opt-find?" b))))

(defcfun opt-find-1 (i r x)
	(while (and (gt (setq i (dec i)) 0)
				(not (match? (defq e (elem i *inst*)) x)))
		(if (or (find r e) (find (elem 0 e) '(vpif else endif))) (setq i 0)))
	(setq p (if (gt i 0) i)))

(defcfun opt-find-2 (i r x y)
	(defq e nil)
	(while (and (gt (setq i (dec i)) 0)
				(not (match? (setq m 0 e (elem i *inst*)) x))
				(not (match? (setq m 1 e e) y)))
		(if (or (find r e) (find (elem 0 e) '(vpif else endif))) (setq i 0)))
	(setq p (if (gt i 0) i)))

(defcfun opt-inst-list ()
	(defq i 0 p nil m nil)
	(while (lt (setq i (inc i)) (length *inst*))
		(cond
			;variable loading and offset folding
			((find (defq e (elem i *inst*) o (elem 0 e))
				'(vp-cpy-ir vp-cpy-ir-b vp-cpy-ir-s vp-cpy-ir-i vp-cpy-ir-ub vp-cpy-ir-us vp-cpy-ir-ui))
				(when (opt-find? i (defq r (elem 1 e)) `(vp-lea-i r4 _ ,r) `(vp-add-cr _ ,r))
					(cond
						((eq m 0)
							(elem-set 1 e r4)
							(elem-set 2 e `(add ,(elem 2 (elem p *inst*)) ,(elem 2 e))))
						((eq m 1)
							(elem-set 2 e `(add ,(elem 1 (elem p *inst*)) ,(elem 2 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;variable writing and offset folding
			((find o '(vp-cpy-ri vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-i))
				(when (opt-find? i (defq r (elem 2 e)) `(vp-lea-i r4 _ ,r) `(vp-add-cr _ ,r))
					(cond
						((eq m 0)
							(elem-set 2 e r4)
							(elem-set 3 e `(add ,(elem 2 (elem p *inst*)) ,(elem 3 e))))
						((eq m 1)
							(elem-set 3 e `(add ,(elem 1 (elem p *inst*)) ,(elem 3 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (pow2 (eval (elem 1 e)))))
				(elem-set 0 e 'vp-shl-cr)
				(elem-set 1 e s)
				(setq i (dec i)))
			;arithmetic reassignment and constant folding
			((defq l '(vp-add-cr vp-sub-cr vp-mul-cr
						vp-and-cr vp-or-cr vp-xor-cr
						vp-shl-cr vp-shr-cr vp-asr-cr) c (find o l))
				(when (opt-find? i (defq r (elem 2 e)) `(vp-cpy-cr _ ,r) `(,(elem c l) _ ,r))
					(cond
						((eq m 0)
							(setq c (elem c '(add sub mul bit-and bit-or bit-xor bit-shl bit-shr bit-asr)))
							(elem-set 0 e 'vp-cpy-cr))
						((eq m 1)
							(setq c (elem c '(add add mul bit-and bit-or bit-xor add add add)))))
					(elem-set 1 e `(,c ,(elem 1 (elem p *inst*)) ,(elem 1 e)))
					(elem-set p *inst* (lambda))))
			;constant propagation
			((defq c (find o '(vp-add-rr vp-sub-rr vp-mul-rr
								vp-and-rr vp-or-rr vp-xor-rr
								vp-shl-rr vp-shr-rr vp-asr-rr)))
				(when (opt-find? i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
					(elem-set 0 e (elem c '(vp-add-cr vp-sub-cr vp-mul-cr
											vp-and-cr vp-or-cr vp-xor-cr
											vp-shl-cr vp-shr-cr vp-asr-cr)))
					(elem-set 1 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* (lambda))
					(setq i (dec i)))))))

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

(defcfun print-inst (_)
	(and (lst? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defcfun assign-asm-to-script (_1 _2 _3)
	(setq *src* _1 *dst* (map trim (split _2 ",")))
	(unless (eq (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(push (clear *inst*) progn)
		(each (lambda (_)
			(set-token-list _)
			(token-to-rpn)
			(compile-rpn-list)
			(compile-assign)) *dst*)
		(when debug_inst
			(print "pre opt: " _1 " -> " _2)
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(each-rev res-reg-def *src*)
		(fill-reg-def _3)
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign-script-to-asm (_1 _2 _3)
	(setq *src* (map trim (split _1 ",")) *dst* _2)
	(unless (eq (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack)
		(push (clear *inst*) progn)
		(each (lambda (_)
			(set-token-list _)
			(token-to-rpn)
			(compile-rpn-list)
			(pop-value)
			(inc-stack)) *src*)
		(when debug_inst
			(print "pre opt: " _1 " -> " _2)
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(each res-reg-def *dst*)
		(fill-reg-def _3)
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign-script-to-script (_1 _2 _3)
	(setq *src* (map trim (split _1 ",")) *dst* (map trim (split _2 ",")))
	(unless (eq (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack)
		(push (clear *inst*) progn)
		(each (lambda (_)
			(set-token-list _)
			(token-to-rpn)
			(compile-rpn-list)) *src*)
		(each-rev (lambda (_)
			(set-token-list _)
			(token-to-rpn)
			(compile-rpn-list)
			(compile-assign)) *dst*)
		(when debug_inst
			(print "pre opt: " _1 " -> " _2)
			(each print-inst *inst*))
		(opt-inst-list)
		(when debug_inst
			(print "post opt:")
			(each print-inst *inst*))
		(fill-reg-def _3)
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign (&optional _1 _2 _3)
	(setd _1 '() _2 '())
	(defq s (str? _1) d (str? _2))
	(cond
		((and s d)
			;script -> script
			(assign-script-to-script _1 _2 _3))
		((and s (not d))
			;script -> asm
			(assign-script-to-asm _1 _2 _3))
		((and (not s) d)
			;asm -> script
			(assign-asm-to-script _1 _2 _3))
		(t
			;asm -> asm
			(setq *src* _1 *dst* _2)
			(map-src-to-dst))))
