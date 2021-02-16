;;;;;;;;;;;;;;;;;;;
; structured coding
;;;;;;;;;;;;;;;;;;;

(defcvar '*switch* nil '*switch_nxt* nil '*switch_stk* (list))

(defmacro beg-sym ()
	'(sym (str "b_" *switch*)))

(defmacro end-sym ()
	'(sym (str "e_" *switch*)))

(defmacro loc-sym (n)
	`(sym (str "o_" *switch* "_" ,n)))

(defmacro cnt-sym ()
	'(sym (str "_c_" *switch*)))

(defmacro llb-sym (_)
	`(sym (cat "_b_" ,_)))

(defmacro lle-sym (_)
	`(sym (cat "_e_" ,_)))

(defmacro errorif (e l)
	(if (> *debug_mode* 0) (list 'gotoif e l)))

(defmacro errorifnot (e l)
	(if (> *debug_mode* 0) (list 'gotoifnot e l)))

(defmacro errorcases (&rest e)
	(if (> *debug_mode* 0) (cat '(progn) e)))

(defun goto (l)
	(vp-jmp l))

(defun gotoif (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 /= 0)))
	(bind '(d o s) e)
	(defq e (find-rev o '(= /= <= >= > <)))
	(case (vp-type s)
		(:c (if e ((eval (elem e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(t (throw "No such compare mode" (vp-type s)))))

(defun gotoifnot (e l)
	(when (str? e)
		(assign e '(r0))
		(setq e '(r0 /= 0)))
	(bind '(d o s) e)
	(defq e (find-rev o '(/= = > < <= >=)))
	(case (vp-type s)
		(:c (if e ((eval (elem e '(vp-beq-cr vp-bne-cr vp-ble-cr vp-bge-cr vp-bgt-cr vp-blt-cr))) s d l)
			(throw "No such comparator" o)))
		(:r (if e ((eval (elem e '(vp-beq-rr vp-bne-rr vp-ble-rr vp-bge-rr vp-bgt-rr vp-blt-rr))) s d l)
			(throw "No such comparator" o)))
		(t (throw "No such compare mode" (vp-type s)))))

(defun switch (&optional _)
	(push *switch_stk* *switch*)
	(setq *switch* *switch_nxt* *switch_nxt* (inc *switch_nxt*))
	(when _
		(defcvar (defq _b (llb-sym _)) (beg-sym) (defq _e (lle-sym _)) (end-sym))
		(push *func_syms* _b _e))
	(defcvar (defq _ (cnt-sym)) 0)
	(push *func_syms* _))

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
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(each! 0 i exitif (list e)))

(defun breakifnot (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(each! 0 i exitifnot (list e)))

(defun continueif (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(each! 0 i repeatif (list e)))

(defun continueifnot (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
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
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(switch l)
	(vp-label (beg-sym))
	(each! 0 i exitifnot (list e)))

(defun loop-whilenot (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(switch l)
	(vp-label (beg-sym))
	(each! 0 i exitif (list e)))

(defun loop-end ()
	(continue)
	(endswitch))

(defun loop-until (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(each! 0 i repeatifnot (list e))
	(endswitch))

(defun loop-untilnot (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(each! 0 i repeatif (list e))
	(endswitch))

(defun vpif (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
	(switch l)
	(each! 0 i nextcaseifnot (list e)))

(defun vpifnot (&rest e)
	(unless (sym? (defq i -2 l (elem -2 e)))
		(setq i -1 l nil))
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

;;;;;;;
; enums
;;;;;;;

(defun def-enum (_1 &optional _2)
	(defcvar '*enum* _1 '*enum_offset* (opt _2 0)))

(defun def-enum-end ()
	(undefc '*enum* '*enum_offset*))

(defun enum (&rest _)
	(each (lambda (_)
		(and (defq d (get (setq _ (sym (cat *enum* "_" _))))) (/= d *enum_offset*)
			(throw "Enum redefined !" _))
		(defcvar _ *enum_offset* '*enum_offset* (inc *enum_offset*))) _))

;;;;;;
; bits
;;;;;;

(defun def-bit (_1 &optional _2)
	(defcvar '*bit* _1 '*bit_offset* (opt _2 0)))

(defun def-bit-end ()
	(undefc '*bit* '*bit_offset*))

(defun bit (&rest _)
	(each (lambda (_)
		(and (defq d (get (setq _ (sym (cat *bit* "_" _))))) (/= d (<< 1 *bit_offset*))
			(throw "Bit redefined !" _))
		(defcvar _ (<< 1 *bit_offset*) '*bit_offset* (inc *bit_offset*))) _))

;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

(defcvar '*sym_sp* 0 '*syms* (list))

(defmacro new-scope ()
	'(push *syms* (list 0 (list) (list))))

(defun get-scope (_)
	(defq o 0)
	(each! (inc _) -2 (lambda (_)
			(setq o (+ o (elem 0 _)))) (list *syms*)) o)

(defun push-scope ()
	(local-align stack_align)
	(elem-set 0 (elem -3 (new-scope)) *sym_sp*)
	(when (/= *sym_sp* 0)
		(when *debug_inst*
			(print "(vp-alloc " *sym_sp* ")"))
		(vp-alloc *sym_sp*)
		(setq *sym_sp* 0)))

(defun pop-scope-syms ()
	(pop *syms*)
	(defq u (list))
	(each! 0 -1 (lambda (a)
		(unless (elem -2 a) (push u (elem _ (elem 1 s))))) (list (elem 2 (defq s (pop *syms*)))))
	(if (/= 0 (length u))
		(throw "Unused symbols !" u))
	(new-scope)
	(setq *sym_sp* 0)
	(elem 0 s))

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
	(if (find-rev (setq _1 (sym _1)) (elem 1 (defq _ (pop *syms*))))
		(throw "Symbol redefined !" _1))
	(push (elem 1 _) _1)
	(push (elem 2 _) (cat (list (length *syms*) _2) _3 '(nil)))
	(push *syms* _))

(defun get-sym (_1)
	;_1 name
	(defq _1 (sym _1) a (some! -2 0 nil (lambda (s)
		(if (defq _ (find-rev _1 (elem 1 s)))
			(elem _ (elem 2 s)))) (list *syms*)))
	(when a (elem-set -2 a t)) a)

(defun used-syms (&rest _)
	(each get-sym _))

(defun operator (_1 _2 &optional _3 _4)
	;_1 name
	;_2 precedence
	;_3 associativity
	;_4 compile macro
	(def-sym _1 'op (* _2 2) (opt _3 0) (opt _4 compile-null)))

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

(defcvar '*struct* nil '*struct_sp* nil)

(defmacro type-sym (_)
	`(sym (str "_t_" ,_)))

(defmacro struct-sym (_)
	`(sym (cat *struct* "_" ,_)))

(defun check-field (_)
	(setq _ (struct-sym _))
	(unless (eql *struct* 'local)
		(and (defq d (get _)) (/= d *struct_sp*)
			(throw "Field redefined !" _))) _)

(defun local-align (&optional _)
	(setd _ +ptr_size+)
	(if *struct*
		(setq *struct_sp* (align *struct_sp* _))
		(setq *sym_sp* (align *sym_sp* _))))

(defun def-struct (s &optional o)
	(setq *struct* s *struct_sp* (if o (eval (sym (cat o "_size"))) 0)))

(defun def-struct-end ()
	(defcvar (sym (cat *struct* "_size")) *struct_sp* '*struct* nil '*struct_sp* nil))

(defmacro def-type (n s y)
	`(defun ,n (&rest f)
		(each (lambda (_)
			(local-align ,s)
			(cond
				(*struct*
					(defcvar (setq _ (check-field _)) *struct_sp* (type-sym _) ,y
						'*struct_sp* (+ *struct_sp* ,s)))
				(t
					(def-sym _ 'var *sym_sp* ,y)
					(setq *sym_sp* (+ *sym_sp* ,s))))) f)))

(def-type byte +byte_size+ "b")
(def-type ubyte +byte_size+ "B")
(def-type short +short_size+ "s")
(def-type ushort +short_size+ "S")
(def-type int +int_size+ "i")
(def-type uint +int_size+ "I")
(def-type long +long_size+ "l")
(def-type ulong +long_size+ "L")
(def-type ptr +ptr_size+ "p")
(def-type pbyte +ptr_size+ "pb")
(def-type pubyte +ptr_size+ "pB")
(def-type pshort +ptr_size+ "ps")
(def-type pushort +ptr_size+ "pS")
(def-type pint +ptr_size+ "pi")
(def-type puint +ptr_size+ "pI")
(def-type plong +ptr_size+ "pl")
(def-type pulong +ptr_size+ "pL")
(def-type pptr +ptr_size+ "pp")

(defun offset (_)
	(cond
		(*struct*
			(defcvar (check-field _) *struct_sp*))
		(t
			(throw "Attempt to define offset into local stack !" _))))

(defun struct (_ s)
	(if (sym? s) (setq s (eval (sym (cat s "_size")))))
	(cond
		(*struct*
			(defcvar (setq _ (check-field _)) *struct_sp* (type-sym _) nil
				'*struct_sp* (+ *struct_sp* s)))
		(t
			(def-sym _ 'var *sym_sp* 0 "")
			(setq *sym_sp* (+ *sym_sp* s)))))

(defmacro union (&rest _)
	(if (notevery list? _)
		(throw "Union needs lists as parameters !" _))
	(cond
		(*struct*
			(defq o *struct_sp* m o)
			(each (lambda (_)
				(setq *struct_sp* o)
				(eval _)
				(setq m (max m *struct_sp*))) _)
			(setq *struct_sp* m))
		(t
			(defq o *sym_sp* m o)
			(each (lambda (_)
				(setq *sym_sp* o)
				(eval _)
				(setq m (max m *sym_sp*))) _)
			(setq *sym_sp* m))))

;;;;;;;;;;;;;;;;;;;;
; parameter handling
;;;;;;;;;;;;;;;;;;;;

(defun assign-ignored-to-asm ()
	(defq s (list) d (list))
	(each! 0 -1 (lambda (x y)
		(unless (or (eql x y) (eql y '_))
			(push s x) (push d y))) (list *src* *dst*))
	(setq *src* s *dst* d))

(defun assign-ignored-to-script ()
	(defq s (list) d (list))
	(each! 0 -1 (lambda (x y)
		(unless (eql y "_")
			(push s x) (push d y))) (list *src* *dst*))
	(setq *src* s *dst* d))

(defun assign-topology-sort ()
	(defq s *src* d *dst* i 0 c 1000 l (dec (length d)))
	(while (and (< i l) (/= c 0))
		(if (setq j (unless (list? (defq j (elem i d)))
				(some! -1 (inc i) nil (lambda (s d)
					(cond
						((and (list? s) (find-rev j s)) _)
						((and (list? d) (find-rev j d)) _)
						((eql j s) _))) (list s d))))
			(setq s (cat (slice 0 i s) (slice j (inc j) s) (slice i j s) (slice (inc j) -1 s))
				d (cat (slice 0 i d) (slice j (inc j) d) (slice i j d) (slice (inc j) -1 d))
				c (dec c))
			(setq i (inc i))))
	(if (= c 0)
		(throw "Copy cycle detected !" (list s d))
		(setq *src* s *dst* d)))

(defun assign-asm-to-asm (*src* *dst*)
	(when (/= (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-asm)
	(when (> (length *dst*) 0)
		(if (> (length *dst*) 1) (assign-topology-sort))
		(each (lambda (x y)
			(case (vp-type x)
				(:r (case (vp-type y)
						(:r (vp-cpy-rr x y))
						(:i (vp-cpy-ri x (elem 0 y) (elem 1 y)))
						(:ii (vp-cpy-ri-i x (elem 0 y) (elem 1 y)))
						(:is (vp-cpy-ri-s x (elem 0 y) (elem 1 y)))
						(:ib (vp-cpy-ri-b x (elem 0 y) (elem 1 y)))
						(:iui (vp-cpy-ri-i x (elem 0 y) (elem 1 y)))
						(:ius (vp-cpy-ri-s x (elem 0 y) (elem 1 y)))
						(:iub (vp-cpy-ri-b x (elem 0 y) (elem 1 y)))
						(:d (vp-cpy-rd x (elem 0 y) (elem 1 y)))
						(:di (vp-cpy-rd-i x (elem 0 y) (elem 1 y)))
						(:ds (vp-cpy-rd-s x (elem 0 y) (elem 1 y)))
						(:db (vp-cpy-rd-b x (elem 0 y) (elem 1 y)))
						(:dui (vp-cpy-rd-i x (elem 0 y) (elem 1 y)))
						(:dus (vp-cpy-rd-s x (elem 0 y) (elem 1 y)))
						(:dub (vp-cpy-rd-b x (elem 0 y) (elem 1 y)))
						(t (throw "Invalid dst parameter !" (list x y)))))
				(:c (vp-cpy-cr x y))
				(:i (vp-cpy-ir (elem 0 x) (elem 1 x) y))
				(:ii (vp-cpy-ir-i (elem 0 x) (elem 1 x) y))
				(:is (vp-cpy-ir-s (elem 0 x) (elem 1 x) y))
				(:ib (vp-cpy-ir-b (elem 0 x) (elem 1 x) y))
				(:iui (vp-cpy-ir-ui (elem 0 x) (elem 1 x) y))
				(:iub (vp-cpy-ir-ub (elem 0 x) (elem 1 x) y))
				(:ius (vp-cpy-ir-us (elem 0 x) (elem 1 x) y))
				(:d (vp-cpy-dr (elem 0 x) (elem 1 x) y))
				(:di (vp-cpy-dr-i (elem 0 x) (elem 1 x) y))
				(:ds (vp-cpy-dr-s (elem 0 x) (elem 1 x) y))
				(:db (vp-cpy-dr-b (elem 0 x) (elem 1 x) y))
				(:dui (vp-cpy-dr-ui (elem 0 x) (elem 1 x) y))
				(:dus (vp-cpy-dr-us (elem 0 x) (elem 1 x) y))
				(:dub (vp-cpy-dr-ub (elem 0 x) (elem 1 x) y))
				(:&i (vp-lea-i (elem 1 x) (elem 2 x) y))
				(:&d (vp-lea-d (elem 1 x) (elem 2 x) y))
				(:@ (fn-bind (elem 1 x) y))
				(:s (fn-string x y))
				(:$ (vp-lea-p (elem 1 x) y))
				(t (throw "Invalid src parameter !" (list x y))))) *src* *dst*)))

;;;;;;;;;;
; tokenize
;;;;;;;;;;

(defmacro is_white_space_char (_)
	`(not (< (ascii-code " ") (code ,_) 127)))

(defmacro is_symbol_char (_)
	(list 'find-rev _ "+ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"))

(defmacro is_path_char (_)
	(list 'find-rev _ "ABCDEFGHIJKLMNOPQRSTUVWXYZ_/0123456789abcdefghijklmnopqrstuvwxyz"))

(defmacro is_label_char (_)
	(list 'find-rev _ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abcdefghijklmnopqrstuvwxyz"))

(defmacro is_numeral_char (_)
	(list 'find-rev _ "0123456789"))

(defmacro is_allowed_number_char (_)
	(list 'find-rev _ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789"))

(defmacro token-push (k c)
	`(push tokens (list ,k ,c)))

(defun token-parse (l)
	(defq m -1 u 1 i 0 p 0 tokens (clear '()))
	(while (< i (length l))
		(defq c (elem i l)
			s (find-rev c {@$"-*~!+.^)(][</%&|=>}) ;"
			s (if s (+ s 2) 0))
		(cond
			((= m 1)
				;symbol mode
				(if (is_symbol_char c)
					(setq c nil)
					(progn
						(if (find-rev (sym (defq s (slice p i l)))
								'(pptr byte ubyte short ushort int uint long ulong ptr))
							(token-push s 5)
							(token-push s m))
						(setq p i u 0 m -1))))
			((= m -1)
				;scanning
				(if (is_white_space_char c)
					(setq p (inc i))
					(cond
						((find-rev c ")(][")
							;)(][
							(token-push c s)
							(setq p (inc i) u (logand (- s 12) 1)))
						((and (= s 9) (= u 1))
							;+XYZ+ symbol !
							(setq m 1))
						((>= s 5)
							;-*~!+.?^</%&|=>
							(setq m s))
						((= s 0)
							;not whitespace
							(setq m (if (is_numeral_char c) 0 1)))
						(t
							;@$?"
							(setq p (inc i) m s))))
				(setq c nil))
			((>= m 5)
				;op mode
				(cond
					((or (is_white_space_char c) (< s 16))
						;white space, )([]
						(cond
							((= u 1)
								;unary operator
								(if (setq s (find-rev (slice p i l) "-*&~!"))
									(token-push (elem s "_#:~!") m)
									(throw "Unknown unary operator !" (slice p i l))))
							(t
								;binary operator
								(token-push (slice p i l) m)
								(setq u 1)))
						(setq p i m -1))
					(t
						(setq c nil))))
			((= m 4)
				;string mode
				(if (/= s 4)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p (inc i) u 0 m -1))))
			((= m 2)
				;bind
				(if (is_path_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			((= m 3)
				;label
				(if (is_label_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			((= m 0)
				;number mode
				(if (is_allowed_number_char c)
					(setq c nil)
					(progn
						(token-push (slice p i l) m)
						(setq p i u 0 m -1))))
			(t (throw "Unknown parse mode !" m)))
		(unless c (setq i (inc i))))
	(unless (= p i)
		(token-push (slice p i l) m)) tokens)

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

(defmacro rpn-push (s v)
	`(push rpn (list (sym ,s) ,v)))

(defun rpn-parse (tokens)
	(defq o '() rpn (clear '()))
	(each (lambda ((k c))
		(cond
			((>= c 5) ;operator
				(cond
					((find-rev k "([")
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
						(defq v (- (elem 2 s) (elem 3 s)) x t)
						(while (and x (/= 0 (length o)))
							(setq s (get-sym (elem -2 o)))
							(if (>= v (elem 2 s))
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

(defcvar '*vreg* '(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14 _v15))
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
	`(defq *vreg* *vreg* *inst* (push (clear '()) progn)
		*vregt* (slice 0 ,_ '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))))

(defun set-reg-map (l &optional _)
	(setd _ '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(each (lambda (_ r) (set (env) _ r)) *vreg* (if (/= 0 (length l)) (merge-obj l _) _)))

(defun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))

(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (slice 1 -1 w) z (if (eql z "") nil z) w (elem 0 w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem (find-rev w "bBsSiIlLp") '(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
										vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)))
	(add-inst (list w x 0 x)))

(defun compile-deref? ()
	(if (get-type)
		(compile-deref)))

(defun pop-value ()
	(compile-deref?)
	(pop *vregt*)
	(vreg-sym (length *vregt*)))

(defun top-value ()
	(when (get-type)
		(compile-deref)
		(set-type nil))
	(top-reg))

(defun compile-null ()
	(throw "Null operator !" nil))

(defun compile-const (_)
	(add-inst (list 'vp-cpy-cr _ (push-reg nil))))

(defun compile-arrow (&optional _)
	(bind '((y w) x) (list (pop-reg) (pop-value)))
	(if _ (push-reg nil))
	(setq w (elem (find-rev w "bBsSiIlLp") '(vp-cpy-ri-b vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-s
										vp-cpy-ri-i vp-cpy-ri-i vp-cpy-ri vp-cpy-ri vp-cpy-ri)))
	(add-inst (list w x y 0)))

(defun compile-ref (_)
	(cond
		((not (defq s (get-sym _)))
			;not in symbol table so figure out what it is
			(cond
				((get (type-sym _))
					;field/member
					(add-inst (list 'vp-cpy-cr _ (push-reg (eval (type-sym _))))))
				((get _)
					;equate
					(compile-const _))
				(t (throw "Symbol not defined !" _))))
		((eql 'var (elem 1 s))
			;variable
			(add-inst (list 'vp-lea-i rsp (+ (get-scope (elem 0 s)) (elem 2 s))
									(push-reg (elem 3 s)))))
		(t (throw "Symbol not a variable !" _))))

(defun compile-cast (_)
	(if (defq c (find-rev (sym _) '(pptr byte ubyte short ushort int uint long ulong ptr)))
		(set-type (elem c '("pp" "b" "B" "s" "S" "i" "I" "l" "L" "p")))
		(throw "Unknown type cast" _)))

(defun compile-member (_)
	(bind '(_ w) (pop-reg))
	(compile-deref)
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-uaddrof (_)
	(set-type nil))

(defun compile-field (_)
	(bind '(_ w) (pop-reg))
	(set-type w)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-index (_)
	(defq _ (pop-value))
	(compile-deref)
	(add-inst (list 'vp-add-rr _ (top-reg))))

(defun compile-uminus (_)
	(add-inst (list 'vp-mul-cr -1 (top-value))))

(defun compile-uderef (_)
	(compile-deref))

(defun compile-unot (_)
	(add-inst (list 'vp-xor-cr -1 (top-value))))

(defun compile-mul (_)
	(add-inst (list 'vp-mul-rr (pop-value) (top-value))))

(defun compile-fmul (_)
	(add-inst
		(list 'vp-mul-rr (pop-value) (defq _ (top-value)))
		(list 'vp-asr-cr 16 _)))

(defun compile-divu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)))

(defun compile-remu (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr-u z _ x)
		(list 'vp-cpy-rr _ x)))

(defun compile-div (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defun compile-fdiv (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-shl-cr 16 x)
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)))

(defun compile-rem (_)
	(defq _ (tmp-reg) z (pop-value) x (top-value))
	(add-inst
		(list 'vp-ext-rr x _)
		(list 'vp-div-rrr z _ x)
		(list 'vp-cpy-rr _ x)))

(defun compile-plus (_)
	(add-inst (list 'vp-add-rr (pop-value) (top-value))))

(defun compile-minus (_)
	(add-inst (list 'vp-sub-rr (pop-value) (top-value))))

(defun compile-lshift (_)
	(add-inst (list 'vp-shl-rr (pop-value) (top-value))))

(defun compile-rshift (_)
	(add-inst (list 'vp-shr-rr (pop-value) (top-value))))

(defun compile-arshift (_)
	(add-inst (list 'vp-asr-rr (pop-value) (top-value))))

(defun compile-cmp (_)
	(add-inst (list (elem (find-rev _ '(= /= <= >= > <))
		'(vp-seq-rr vp-sne-rr vp-sle-rr vp-sge-rr vp-sgt-rr vp-slt-rr))
		(pop-value) (top-value))))

(defun compile-and (_)
	(add-inst (list 'vp-and-rr (pop-value) (top-value))))

(defun compile-xor (_)
	(add-inst (list 'vp-xor-rr (pop-value) (top-value))))

(defun compile-or (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-ulnot (_)
	(add-inst (list 'vp-lnot-rr (tmp-reg) (top-value))))

(defun compile-land (_)
	(add-inst (list 'vp-land-rr (pop-value) (top-value))))

(defun compile-lor (_)
	(add-inst (list 'vp-or-rr (pop-value) (top-value))))

(defun compile-operator (_)
	(cond
		((not (defq s (get-sym _)))
			(throw "Operator not defined !" _))
		((not (eql 'op (elem 1 s)))
			(throw "Not an operator !" _))
		(t
			((elem 4 s) _))))

(defun compile-string (_)
	(add-inst (list 'fn-string _ (push-reg nil))))

(defun compile-bind (_)
	(add-inst `(fn-bind ',_ ,(push-reg nil))))

(defun compile-label (_)
	(add-inst `(vp-lea-p ',_ ,(push-reg nil))))

(defun compile-rpn-list (rpn)
	(each (lambda ((k c))
		(if (<= -1 c 4)
			(eval (elem (inc c)
				'((compile-operator k)
				(compile-const (str-to-num k))
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
(operator "pptr" 0 1 compile-cast)
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
(operator "=" 7 0 compile-cmp)
(operator "/=" 7 0 compile-cmp)
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
	''(+ - * logand logior logxor << >> >>>))

(defmacro opt-fold-cr-ops ()
	''(+ + * logand logior logxor + + +))

(defmacro opt-cr-fuse-ops ()
	''(vp-add-cr vp-sub-cr))

(defmacro opt-uses (r _)
	`(find-rev ,r ,_))

(defun opt-find-1 (_ r x)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (/= _ -1) _)))

(defun opt-find-2 (_ r x y)
	(setq _ (some! _ 0 nil (lambda (e)
				(cond
					((match? e x) (setq m 0) _)
					((match? e y) (setq m 1) _)
					((opt-uses r e) -1))) (list *inst*))
		p (and _ (/= _ -1) _)))

(defun opt-inst-list ()
	(defq i 0 p nil m nil)
	(while (< (setq i (inc i)) (length *inst*))
		(cond
			;variable loading and offset folding
			((find-rev (defq e (elem i *inst*) o (elem 0 e)) (opt-read-ops))
				(when (opt-find-2 i (defq r (elem 1 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 1 e rsp)
							(elem-set 2 e `(+ ,(elem 2 (elem p *inst*)) ,(elem 2 e))))
						((= m 1)
							(elem-set 2 e `(+ ,(elem 1 (elem p *inst*)) ,(elem 2 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;variable writing and offset folding
			((find-rev o (opt-write-ops))
				(when (opt-find-2 i (defq r (elem 2 e)) `(vp-lea-i rsp _ ,r) `(vp-add-cr _ ,r))
					(cond
						((= m 0)
							(elem-set 2 e rsp)
							(elem-set 3 e `(+ ,(elem 2 (elem p *inst*)) ,(elem 3 e))))
						((= m 1)
							(elem-set 3 e `(+ ,(elem 1 (elem p *inst*)) ,(elem 3 e)))
							(setq i (dec i))))
					(elem-set p *inst* (lambda))))
			;strength reduction
			((and (eql o 'vp-mul-cr) (defq s (log2 (eval (elem 1 e)))))
				(elem-set 0 e 'vp-shl-cr)
				(elem-set 1 e s)
				(setq i (dec i)))
			;constant propagation
			((defq c (find-rev o (opt-rr-ops)))
				(when (opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
					(elem-set 0 e (elem c (opt-cr-ops)))
					(elem-set 1 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* (lambda))
					(setq i (dec i))))
			;arithmetic reassignment and constant folding
			((and (defq c (find-rev o (opt-cr-ops)))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-cpy-cr _ ,r) (list o '_ r)))
					(cond
						((= m 0)
							(setq c (elem c (opt-fold-cpy-ops)))
							(elem-set 0 e 'vp-cpy-cr))
						((= m 1)
							(setq c (elem c (opt-fold-cr-ops)))))
					(elem-set 1 e (list c (elem 1 (elem p *inst*)) (elem 1 e)))
					(elem-set p *inst* (lambda)))
			;constant fusion
			((and (defq c (find-rev o (opt-cr-fuse-ops)))
				(opt-find-2 i (defq r (elem 2 e)) `(vp-add-cr _ ,r) `(vp-sub-cr _ ,r)))
					(cond
						((= m 0)
							(setq c (elem c '(+ -))))
						((= m 1)
							(setq c (elem c '(- +)))))
					(elem-set 1 e (list c (elem 1 e) (elem 1 (elem p *inst*))))
					(elem-set p *inst* (lambda)))
			;compare constant forwarding
			((and (defq c (find-rev o (opt-sxx-rr-ops)))
				(opt-find-1 i (defq r (elem 1 e)) `(vp-cpy-cr _ ,r))
				(<= -0x80000000 (defq v (eval (elem 1 (elem p *inst*)))) 0x7fffffff))
					(elem-set 0 e (elem c (opt-sxx-cr-ops)))
					(elem-set 1 e v)
					(elem-set p *inst* (lambda)))
			)))

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

(defun print-inst (_)
	(and (list? _) (not (eql (elem 0 _) 'lambda)) (print (ascii-char 9) _)))

(defun assign-asm-to-script (*src* *dst* _)
	(unless (= (length *src*) (length (setq *dst* (map trim (split *dst* ",")))))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-script)
	(when (/= 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(each! 0 -1 (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-arrow)) (list *dst*))
		(when *debug_inst*
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when *debug_inst*
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map (reverse *src*) _)
		(eval *inst* *compile_env*)))

(defun assign-script-to-asm (*src* *dst* _)
	(unless (= (length (setq *src* (split *src* ","))) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(when (/= 0 (length *dst*))
		(reset-reg-stack 0)
		(each! 0 -1 (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-deref?)) (list *src*))
		(when *debug_inst*
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when *debug_inst*
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map (cat *dst*) _)
		(eval *inst* *compile_env*)))

(defun assign-script-to-script (*src* *dst* _)
	(setq *src* (split *src* ",") *dst* (map trim (split *dst* ",")))
	(unless (= (length *src*) (length *dst*))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-script)
	(when (/= 0 (length *dst*))
		(reset-reg-stack 0)
		(each! 0 -1 (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))) (list *src*))
		(each! -1 0 (lambda (_)
			(compile-rpn-list (rpn-parse (token-parse _)))
			(compile-arrow)) (list *dst*))
		(when *debug_inst*
			(print "pre opt:")
			(each print-inst *inst*))
		(opt-inst-list)
		(when *debug_inst*
			(print "post opt:")
			(each print-inst *inst*))
		(set-reg-map '() _)
		(eval *inst* *compile_env*)))

(defun assign (&optional _1 _2 _3)
	;optional src, dst, compiler regs
	(if (str? (setq _1 (opt _1 '())))
		(if (str? (setq _2 (opt _2 '())))
			(assign-script-to-script _1 _2 _3)
			(assign-script-to-asm _1 _2 _3))
		(if (str? (setq _2 (opt _2 '())))
			(assign-asm-to-script _1 _2 _3)
			(assign-asm-to-asm _1 _2))))
