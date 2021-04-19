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

(defun def-enum (size)
	(push syms (sym (cat name "_" field)))
	(push values base)
	(setq base (+ base size)))

(defmacro def-enum (name base &rest lines)
	; (def-enum name base [(enum field ...)] ...)
	(if (def? (sym (cat name "_size")) *compile_env*) (throw "Enum redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! 1 -1 (lambda (field)
			(case (elem 0 line)
				(enum (def-enum +byte_size))))
			(list line))) lines)
	(push syms (sym (cat name "_size")))
	(push values base)
	`(eval '(bind ',syms ',values) *compile_env*))

;;;;;;
; bits
;;;;;;

(defun def-bit ()
	(push syms (sym (cat name "_" field)))
	(push values (<< 1 base))
	(setq base (inc base)))

(defmacro def-bit (name base &rest lines)
	; (def-bit name base [(bit field ...)] ...)
	(if (def? (sym (cat name "_size")) *compile_env*) (throw "Bit redefined !" name))
	(defq syms (list) values (list) base (eval base))
	(each (lambda (line)
		(each! 1 -1 (lambda (field)
			(case (elem 0 line)
				(bit (def-bit))))
			(list line))) lines)
	(push syms (sym (cat name "_size")))
	(push values base)
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
	(push values base)
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
			(setq o (+ o (elem 0 _)))) (list *syms*)) o)

(defun push-scope ()
	(setq *sym_sp* (align *sym_sp* stack_align))
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
	;_1 symbol
	(defq a (some! -2 0 nil (lambda (s)
		(if (defq _ (find-rev _1 (elem 1 s)))
			(elem _ (elem 2 s)))) (list *syms*)))
	(when a (elem-set -2 a t)) a)

(defmacro used-syms (&rest _)
	(each get-sym _) nil)

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
		(case (elem 0 line)
			(align
				(if (> (length line) 1)
					(push prog `(setq *sym_sp* (align *sym_sp* ,(eval (elem 1 line)))))
					(push prog '(setq *sym_sp* (align *sym_sp* +long_size)))))
			(struct
				(push prog
					`(def-sym ',(elem 1 line) 'var *sym_sp* 0 "")
					`(setq *sym_sp* (+ *sym_sp* ,(eval (elem 2 line))))))
			(union
				(defq o (gensym) m (gensym))
				(push prog `(defq ,o *sym_sp* ,m *sym_sp*))
				(each! 1 -1 (lambda (_)
					(push prog
						`(def-vars ,_)
						`(setq ,m (max ,m *sym_sp*) *sym_sp* ,o))) (list line))
				(push prog `(setq *sym_sp* ,m)))
			(t
				(if (defq alignment (find (elem 0 line)
						'(short int long ushort uint ulong ptr pptr
							pubyte pushort puint pulong pbyte pshort pint plong)))
					(push prog
						`(setq *sym_sp* (align *sym_sp* ,(elem alignment
							'(+short_size +int_size +long_size +short_size
								+int_size +long_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size
								+ptr_size +ptr_size +ptr_size +ptr_size))))))
				(each! 1 -1 (lambda (var)
					(case (elem 0 line)
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

;;;;;;;;;
; cscript
;;;;;;;;;

(include "lib/asm/cscript.inc")

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

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

(defun print-inst (_)
	(and (list? _) (not (eql (elem 0 _) 'lambda)) (print (ascii-char 9) _)))

(defun assign-asm-to-script (*src* *dst* _)
	(unless (= (length *src*) (length (setq *dst* (map trim (split *dst* ",")))))
		(throw "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(assign-ignored-to-script)
	(when (/= 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(each! 0 -1 (lambda (_)
			(cscript-compile (cscript-reverse-polish (cscript-tokenize _)))
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
			(cscript-compile (cscript-reverse-polish (cscript-tokenize _)))
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
			(cscript-compile (cscript-reverse-polish (cscript-tokenize _)))) (list *src*))
		(each! -1 0 (lambda (_)
			(cscript-compile (cscript-reverse-polish (cscript-tokenize _)))
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
