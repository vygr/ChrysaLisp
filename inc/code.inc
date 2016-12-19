;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

(defcvar '*switch* nil '*switch-nxt* nil '*switch-stk* (list))

(defmacro beg-sym ()
	'(sym (cat "_beg_" (str *switch*))))

(defmacro end-sym ()
	'(sym (cat "_end_" (str *switch*))))

(defmacro loc-sym (n)
	`(sym (cat "_loc_" (str *switch*) "_" (str ,n))))

(defmacro cnt-sym ()
	'(sym (cat "_cnt_" (str *switch*))))

(defcfun goto (l)
	(vp-jmp l))

(defcfun gotoif (e l)
	(assign e '(r0))
	(vp-jmpif '(r0 != 0) l))

(defcfun gotoifnot (e l)
	(assign e '(r0))
	(vp-jmpifnot '(r0 != 0) l))

(defcfun switch ()
	(push *switch-stk* *switch*)
	(setq *switch* *switch-nxt* *switch-nxt* (inc *switch-nxt*))
	(defcvar (cnt-sym) 0)
	(vp-label (beg-sym)))

(defcfun default ()
	(defq s (cnt-sym) c (eval s))
	(vp-label (loc-sym c))
	(set s (inc c)))

(defcfun endswitch ()
	(vp-label (end-sym))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcfun case (e)
	(default)
	(defq c (eval (cnt-sym)) l (loc-sym c))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(progn
			(vp-jmpifnot `(~e) l))))

(defcfun casenot (e)
	(default)
	(defq c (eval (cnt-sym)) l (loc-sym c))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(progn
			(vp-jmpif `(~e) l))))

(defcfun break ()
	(vp-jmp (end-sym)))

(defcfun breakif (e)
	(defq l (end-sym))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun breakifnot (e)
	(defq l (end-sym))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcfun continue ()
	(vp-jmp (beg-sym)))

(defcfun continueif (e)
	(defq l (beg-sym))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun continueifnot (e)
	(defq l (beg-sym))
	(if (str? e)
		(progn
			(assign e '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcfun loop-start ()
	(switch))

(defcfun loop-while (e)
	(switch)
	(breakifnot e))

(defcfun loop-whilenot (e)
	(switch)
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
	(case e))

(defcfun vpifnot (e)
	(switch)
	(casenot e))

(defcfun elseif (e)
	(break)
	(case e))

(defcfun else ()
	(break)
	(default))

(defcfun endif ()
	(endswitch))

;;;;;;;
; enums
;;;;;;;

(defcvar '*enum* nil)

(defcfun def-enum (&optional b)
	(setq *enum* (if b b 0)))

(defcfun def-enum-end ()
	(setq *enum* nil))

(defcfun enum (b)
	(defcvar b *enum*)
	(setq *enum* (inc *enum*)))

;;;;;;
; bits
;;;;;;

(defcvar '*bit* nil)

(defcfun def-bit (&optional b)
	(setq *bit* (if b b 0)))

(defcfun def-bit-end ()
	(setq *bit* nil))

(defcfun bit (b)
	(defcvar b (bit-shl 1 *bit*))
	(setq *bit* (inc *bit*)))

;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

(defcvar '*scopes* (list) '*symbols* (list) '*var-sp* 0)

(defcfun push-scope ()
	(local-align)
	(push *scopes* (list (length *symbols*) *var-sp*))
	(when (ne *var-sp* 0)
		(when debug_emit
			(print "(vp-sub-cr " *var-sp* " " r4 ")"))
		(vp-sub-cr *var-sp* r4)
		(setq *var-sp* 0)))

(defcfun pop-scope ()
	(setq *var-sp* (elem 1 (pop *scopes*)))
	(when (ne (length *scopes*) 0)
		(setq *symbols* (slice 0 (elem 0 (elem -2 *scopes*)) *symbols*)))
	(when (ne *var-sp* 0)
		(when debug_emit
			(print "(vp-add-cr " *var-sp* " " r4 ")"))
		(vp-add-cr *var-sp* r4)
		(setq *var-sp* 0)))

(defcfun unwind ()
	(defq o (get-scope-offset 0))
	(when (ne o 0)
		(when debug_emit
			(print "(vp-add-cr " o " " r4 ")"))
		(vp-add-cr o r4)))

(defcfun return ()
	(unwind)
	(when debug_emit
		(print "(vp-ret)"))
	(vp-ret))

(defcfun get-scope-offset (s)
	(defq o 0)
	(if (ge s (length *scopes*))
		(error "Symbol not in scope !" s)
		(progn
			(defq i (length *scopes*))
			(while (lt s (setq i (dec i)))
				(setq o (add o (elem 1 (elem i *scopes*)))))))
	o)

(defcfun def-sym (%1 %2 %3 &optional %4 %5)
	;%1 name
	;%2 type
	;%3 value1
	;%4 value2
	;%5 aux data
	(setq %1 (sym %1) %4 (if %4 %4 0) %5 (if %5 %5 nil))
	(defq s (elem 0 (elem -2 *scopes*)))
	(while (ne s (length *symbols*))
		(if (eql %1 (elem 1 (elem s *symbols*)))
			(error "Symbol redefined !" %1))
		(setq s (inc s)))
	(push *symbols* (list (length *scopes*) %1 %2 %3 %4 %5)))

(defcfun get-sym (%1)
	;%1 name
	(defq %1 (sym %1) i (length *symbols*) s nil)
	(while (and (ge (setq i (dec i)) 0) (not (eql %1 (elem 1 (setq s (elem i *symbols*)))))))
	(if (ne i -1) s))

(defcfun const (%1 %2)
	;%1 name
	;%2 value
	(def-sym %1 'const %2))

(defcfun operator (%1 %2 &optional %3 %4)
 	;%1 name
	;%2 precidence
	;%3 asociativity
	;%4 compile macro
	(setq %3 (if %3 %3 0) %4 (if %4 %4 compile-null))
	(def-sym %1 'op (mul %2 2) %3 %4))

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

(defcvar '*struct* nil '*struct-offset* nil
	'null_size 0 'byte_size 1 'short_size 2 'int_size 4 'long_size 8 'ptr_size 8)

(defmacro type-sym (s)
	`(sym (cat "_t_" (str ,s))))

(defcfun local-align (&optional x)
	(setq x (if x x long_size))
	(if *struct*
		(setq *struct-offset* (align *struct-offset* x))
		(setq *var-sp* (align *var-sp* x))))

(defcfun def-struct (s &optional o)
	(setq *struct* s *struct-offset* (eval (sym (cat (str (if o o "null")) "_size")))))

(defcfun def-struct-end ()
	(local-align ptr_size)
	(defcvar (sym (cat (str *struct*) "_size")) *struct-offset*)
	(setq *struct* nil *struct-offset* nil))

(defmacro def-type (n s y)
	`(defcfun ,n (&rest f)
		(each (lambda (x)
			(local-align ,s)
			(cond
				(*struct*
					(defcvar x *struct-offset* (type-sym x) ,y)
					(setq *struct-offset* (add *struct-offset* ,s)))
				(t
					(def-sym x 'var *var-sp* 0 ,y)
					(setq *var-sp* (add *var-sp* ,s))))) f)))

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

(defcfun offset (f)
	(if *struct*
		(defcvar f *struct-offset*)
		(error "Atempt to define offset into local stack !" f)))

(defcfun struct (f s)
	(defq s (eval (sym (cat (str s) "_size"))))
	(cond
		(*struct*
			(defcvar f *struct-offset* (type-sym f) nil)
			(setq *struct-offset* (add *struct-offset* s)))
		(t
			(def-sym f 'var *var-sp* 0 "")
			(setq *var-sp* (add *var-sp* s)))))

;;;;;;;;;;;;;;;;;;;;
; paramater handling
;;;;;;;;;;;;;;;;;;;;

(defcvar '*src* nil '*dst* nil)

(defcfun map-remove-ignored ()
	(defq s (list) d (list))
	(each (lambda (x y) (unless (or (eql x y) (eql y '_)  (eql y "_")) (push s x) (push d y))) *src* *dst*)
	(setq *src* s *dst* d))

(defcfun match-any (d s)
	(defq i (list? d) j (list? s))
	(cond
		(i (cond
				(j (error "Ilegal src/dst !" (list s d)))
				(t (find s d))))
		(j (cond
				(i (error "Ilegal src/dst !" (list s d)))
				(t (find d s))))
		(t (eql s d))))

(defcfun find-later-src (d)
	(defq i -1 j (length *src*) d (elem d *dst*))
	(while (ne (setq j (dec j)) -1)
		(if (match-any d (elem j *src*))
			(setq i j j 0)))
	i)

(defcfun map-rotate (i j)
	(setq *src* (cat (slice 0 i *src*) (slice j (inc j) *src*)
					(slice i j *src*) (slice (inc j) (length *src*) *src*))
		*dst* (cat (slice 0 i *dst*) (slice j (inc j) *dst*)
					(slice i j *dst*) (slice (inc j) (length *dst*) *dst*))))

(defcfun map-topology-sort ()
	(defq i -1 c 1000 j nil)
	(while (and (ne (setq i (inc i)) (length *dst*)) (ne c 0))
		(setq j (find-later-src i))
		(if (gt j i)
			(progn
				(map-rotate i j)
				(setq c (dec c)))))
	(when (ne i (length *dst*))
		(error "Copy cycle detected !" (list *src* *dst*))))

(defcfun map-src-to-dst ()
	(when (ne (length *src*) (length *dst*))
		(error "Mismatching number of src/dst parameters !" (list *src* *dst*)))
	(map-remove-ignored)
	(map-topology-sort)
	(each (lambda (x y)
		(defq i (type x))
		(cond
			((eql i 'r)
				(setq i (type y))
				(cond
					((eql i 'r) (vp-cpy-rr x y))
					((eql i 'i) (vp-cpy-ri x (elem 0 y) (elem 1 y)))
					((eql i 'd) (vp-cpy-rd x (elem 0 y) (elem 1 y)))
					(t (error "Invalid dst paramater !" i))))
			((eql i 'c) (vp-cpy-cr x y))
			((eql i 'i) (vp-cpy-ir (elem 0 x) (elem 1 x) y))
			((eql i 'd) (vp-cpy-dr (elem 0 x) (elem 1 x) y))
			((eql i '&i) (vp-lea-i (elem 1 x) (elem 2 x) y))
			((eql i '&d) (vp-lea-d (elem 1 x) (elem 2 x) y))
			((eql i 's) (fn-string x y))
			((eql i '@) (fn-bind (elem 1 x) y))
			((eql i '$) (vp-lea-p (elem 1 x) y))
			(t (error "Invalid src paramater !" i)))) *src* *dst*))

;;;;;;;;;;;
; tokenizer
;;;;;;;;;;;

(defcvar '*token* nil)

(defmacro push-token (&rest b)
	`(push *token* (list ~b)))

(defcfun set-token-list (l)
	(setq *token* (list))
	(defq m -1 u 1 i 0 p "" s nil c nil)
	(while (lt i (length l))
		(setq c (elem i l) s (find c {@$"-*~!+.?^</%&|=> )(][}) s (if s (inc s) 0)) ;"
		(cond
			((eq m -1)
				;scanning
				(unless (eql c " ")
					(cond
						((ge s 20)
							;)(][
							(push-token c s)
							(setq u (bit-and (sub s 20) 1)))
						((or (eq s 0) (ge s 4))
							;-*~!+.?^</%&|=> or symbol
							(setq p (cat p c) m s))
						(t
							;@$"
							(setq m s))))
				(setq c nil))
			((gt m 3)
				;op mode
				(cond
					((or (gt s 18) (lt s 12))
						;space, )([] or not </%&|=>
						(cond
							((eq u 1)
								;unary operator
								(if (setq s (find p "-*&~!"))
									(push-token (elem s "_#:~!") m)
									(error "Unknown unary operator !" p)))
							(t
								;binary operator
								(push-token p m)
								(setq u 1)))
						(setq p "" m -1))
					(t
						(setq p (cat p c) c nil))))
			((lt m 3)
				;symbol mode
				(if (or (eq s 0) (eq s 13))
					(setq p (cat p c) c nil)
					(progn
						(push-token p m)
						(setq p "" u 0 m -1))))
			(t
				;string mode
				(if (ne s 3)
					(setq p (cat p c) c nil)
					(progn
						(push-token p m)
						(setq p "" u 0 m -1)))))
		(unless c (setq i (inc i))))
	(unless (eql p "")
		(push-token p m)))

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

(defcvar '*rpn* nil)

(defmacro push-rpn (s v)
	`(push *rpn* (list (sym ,s) ,v)))

(defcfun token-to-rpn ()
	(setq *rpn* (list))
	(defq o (list) i 0 k nil c nil s nil x nil v nil)
	(while (lt i (length *token*))
		(setq k (elem 0 (elem i *token*)) c (elem 1 (elem i *token*)))
		(cond
			((gt c 3)
				;operator
				(cond
					((eql k "(")
						(push o k))
					((eql k "[")
						(push o k))
					((eql k ")")
						(while (and (not (eql "(" (setq k (pop o)))) k)
							(push-rpn k -1)))
					((eql k "]")
						(while (and (not (eql "[" (setq k (pop o)))) k)
							(push-rpn k -1))
						(push-rpn "[]" -1))
					(t
						;precidence
						(setq s (get-sym k))
						(if (not s)
							(error "Operator not defined !" k))
						(setq v (sub (elem 3 s) (elem 4 s)) x t)
						(while (and x (ne 0 (length o)))
							(setq s (get-sym (elem -2 o)))
							(if (ge v (elem 3 s))
								(push-rpn (pop o) -1)
								(setq x nil)))
						(push o k))))
			(t
				;string or symbol
				(push-rpn k c)))
		(setq i (inc i)))
	(each-rev (lambda (x)
		(push-rpn x -1)) o))

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

(defcvar '*s* nil '*r* nil '*inst* nil
	'_v0 nil '_v1 nil '_v2 nil '_v3 nil '_v4 nil '_v5 nil '_v6 nil '_v7 nil
	'_v8 nil '_v9 nil '_v10 nil '_v11 nil '_v12 nil '_v13 nil '_v14 nil '_v15 nil
	'_t_0 nil '_t_1 nil '_t_2 nil '_t_3 nil '_t_4 nil '_t_5 nil '_t_6 nil '_t_7 nil
	'_t_8 nil '_t_9 nil '_t_10 nil '_t_11 nil '_t_12 nil '_t_13 nil '_t_14 nil '_t_15 nil)

(defmacro vreg-sym (n)
	`(sym (cat "_v" (str ,n))))

(defmacro inc-stack ()
	'(setq *s* (inc *s*)))

(defmacro dec-stack ()
	'(setq *s* (dec *s*)))

(defmacro nil-type ()
	'(set (type-sym *s*) nil))

(defmacro add-inst (&rest b)
	`(push *inst* ~b))

(defcfun print-reg-stack ()
	(defq i -1)
	(while (lt (setq i (inc i)) 16)
		(print (vreg-sym i) " -> " (eval (type-sym i)))))

(defcfun reset-reg-stack (&optional %1)
	;%1 = reserved slots
	(setq %1 (if %1 %1 0) *s* -1 *r* 0)
	(while (lt (inc-stack) %1)
		(nil-type)))

(defcfun res-reg-def (%1)
	;%1 = 'rX'
	(set (vreg-sym *r*) %1 (type-sym *r*) nil)
	(setq *r* (inc *r*)))

(defcfun add-reg-def (%1)
	;%1 = rX
	(defq i -1)
	(while (and (lt (setq i (inc i)) *r*) (not (eql (eval (vreg-sym i)) %1))))
	(when (eq i *r*)
		(set (vreg-sym i) %1)
		(setq *r* (inc *r*))))

(defcfun fill-reg-def ()
	(each add-reg-def '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)))

(defcfun undef-reg-def ()
	(defq i -1)
	(while (lt (setq i (inc i)) *r*)
		(set (vreg-sym i) nil (type-sym i) nil)))

(defcfun pop-type (s)
	(defq u (type-sym s) w (eval u))
	(if w
		(progn
			(defq v (slice 1 -1 w) v (if (eql v "") nil v))
			(set u v)
			(elem 0 w))
		(error "No type info !" u)))

(defcfun pop-value ()
	(dec-stack)
	(compile-deref?)
	(nil-type)
	(vreg-sym *s*))

(defcfun compile-null ()
	(error "Null operator !" nil))

(defcfun compile-const (%1)
	(nil-type)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-cpy-cr ,%1 ,x)))

(defcfun compile-assign ()
	(defq y (vreg-sym (dec-stack)) w (pop-type *s*) x (pop-value))
	(cond
		((eql w "b")
			(add-inst `(vp-cpy-ri-b ,x ,y 0)))
		((eql w "B")
			(add-inst `(vp-cpy-ri-b ,x ,y 0)))
		((eql w "s")
			(add-inst `(vp-cpy-ri-s ,x ,y 0)))
		((eql w "S")
			(add-inst `(vp-cpy-ri-s ,x ,y 0)))
		((eql w "i")
			(add-inst `(vp-cpy-ri-i ,x ,y 0)))
		((eql w "I")
			(add-inst `(vp-cpy-ri-i ,x ,y 0)))
		(t
			(add-inst `(vp-cpy-ri ,x ,y 0)))))

(defcfun compile-ref (%1)
	(defq s (get-sym %1))
	(cond
		((not s)
			;not in symbol table so figure out what it is
			(cond
				((le (ascii "0") (code (elem 0 %1)) (ascii "9"))
					;number
					(compile-const (to-num %1)))
				((eql %1 'RXX)
					;reserved slot
					(nil-type))
				((def? (type-sym %1))
					;field/member
					(set (type-sym *s*) (eval (type-sym %1)))
					(defq x (vreg-sym *s*))
					(add-inst `(vp-cpy-cr ,%1 ,x)))
				(t
					;equate
					(compile-const %1))))
		((eql 'const (elem 2 s))
			;constant
			(compile-const (elem 3 s)))
		((eql 'var (elem 2 s))
			;variable
			(set (type-sym *s*) (elem 5 s))
			(setq s (add (get-scope-offset (elem 0 s)) (elem 3 s)))
			(defq x (vreg-sym *s*))
			(add-inst `(vp-lea-i r4 ,s ,x)))))

(defcfun compile-deref ()
	(defq v (vreg-sym *s*) w (pop-type *s*))
	(cond
		((eql w "b")
			(add-inst `(vp-cpy-ir-b ,v 0 ,v)))
		((eql w "B")
			(add-inst `(vp-cpy-ir-ub ,v 0 ,v)))
		((eql w "s")
			(add-inst `(vp-cpy-ir-s ,v 0 ,v)))
		((eql w "S")
			(add-inst `(vp-cpy-ir-us ,v 0 ,v)))
		((eql w "i")
			(add-inst `(vp-cpy-ir-i ,v 0 ,v)))
		((eql w "I")
			(add-inst `(vp-cpy-ir-ui ,v 0 ,v)))
		(t
			(add-inst `(vp-cpy-ir ,v 0 ,v)))))

(defcfun compile-deref? ()
	(when (eval (type-sym *s*))
		(compile-deref)))

(defcfun compile-member (o)
	(defq s (dec *s*) x (vreg-sym (setq *s* (sub *s* 2))) y (vreg-sym s))
	(compile-deref)
	(add-inst `(vp-lea-d ,x ,y ,x))
	(set (type-sym *s*) (eval (type-sym s))))

(defcfun compile-uaddrof (o)
	(set (type-sym (dec-stack)) nil))

(defcfun compile-field (o)
	(defq s (dec *s*) x (vreg-sym (setq *s* (sub *s* 2))) y (vreg-sym s))
	(add-inst `(vp-lea-d ,x ,y ,x))
	(set (type-sym *s*) (eval (type-sym s))))

(defcfun compile-index (o)
	(defq y (pop-value))
	(dec-stack)
	(compile-deref)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-lea-d ,x ,y ,x)))

(defcfun compile-uminus (o)
	(defq x (pop-value))
	(add-inst `(vp-mul-cr -1 ,x)))

(defcfun compile-uderef (o)
	(dec-stack)
	(compile-deref)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-lea-i ,x 0 ,x)))

(defcfun compile-unot (o)
	(defq x (pop-value))
	(add-inst `(vp-xor-cr -1 ,x)))

(defcfun compile-mul (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-mul-rr ,y ,x)))

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

(defcfun compile-rem (o)
	(defq y (vreg-sym *s*) z (pop-value) x (pop-value))
	(add-inst
		`(vp-ext-rr ,x ,y)
		`(vp-div-rrr ,z ,y ,x)
		`(vp-cpy-rr ,y ,x)))

(defcfun compile-plus (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-add-rr ,y ,x)))

(defcfun compile-minus (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-sub-rr ,y ,x)))

(defcfun compile-lshift (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-shl-rr ,y ,x)))

(defcfun compile-rshift (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-shr-rr ,y ,x)))

(defcfun compile-arshift (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-asr-rr ,y ,x)))

(defcfun compile-branch (o)
	(defq y (pop-value) x (pop-value))
	(add-inst
		`(vpif (list ,x ',(sym o) ,y))
			`(vp-cpy-cr 1 ,x)
		'(else)
			`(vp-xor-rr ,x ,x)
		'(endif)))

(defcfun compile-and (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-and-rr ,y ,x)))

(defcfun compile-xor (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-xor-rr ,y ,x)))

(defcfun compile-or (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-or-rr ,y ,x)))

(defcfun compile-land (o)
	(defq y (pop-value) x (pop-value))
	(add-inst
		`(vpif (list ,y '!= 0))
			`(vpif (list ,x '!= 0))
				`(vp-cpy-cr 1 ,x)
			'(else)
				`(vp-xor-rr ,x ,x)
			'(endif)
		'(else)
			`(vp-xor-rr ,x ,x)
		'(endif)))

(defcfun compile-lor (o)
	(defq y (pop-value) x (pop-value))
	(add-inst `(vp-or-rr ,y ,x)))

(defcfun compile-ulnot (o)
	(defq x (pop-value))
	(add-inst
		`(vpif (list ,x '== 0))
			`(vp-cpy-cr 1 ,x)
		'(else)
			`(vp-xor-rr ,x ,x)
		'(endif)))

(defcfun compile-ternary (o)
	(defq z (pop-value) y (pop-value) x (pop-value))
	(add-inst
		`(vpif (list ,x '!= 0))
			`(vp-cpy-rr ,y ,x)
		'(else)
			`(vp-cpy-rr ,z ,x)
		'(endif)))

(defcfun compile-operator (%1)
	(defq s (get-sym %1))
	(cond
		((not s)
			(error "Operator not defined !" %1))
		((not (eql 'op (elem 2 s)))
			(error "Not an operator !" %1))
		(t
			((elem 5 s) %1))))

(defcfun compile-string (%1)
	(nil-type)
	(defq x (vreg-sym *s*))
	(add-inst `(fn-string ,%1 ,x)))

(defcfun compile-bind (%1)
	(nil-type)
	(defq x (vreg-sym *s*))
	(add-inst `(fn-bind ',%1 ,x)))

(defcfun compile-label (%1)
	(nil-type)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-lea-p ',%1 ,x)))

(defcfun compile-rpn-list ()
	(defq i 0 k nil c nil)
	(while (lt i (length *rpn*))
		(setq k (elem 0 (elem i *rpn*)) c (elem 1 (elem i *rpn*)))
		(cond
			((eq c -1)
				;operator
				(compile-operator k))
			((eq c 0)
				;...
				(compile-ref k))
			((eq c 1)
				;@
				(compile-bind k))
			((eq c 2)
				;$
				(compile-label k))
			((eq c 3)
				;"..."
				(compile-string (str k)))
			(t
				(error "Unknown token type !" k)))
		(setq *s* (inc *s*) i (inc i))))

(push-scope)
(operator "." 1 0 compile-field)
(operator "->" 1 0 compile-member)
(operator "[]" 1 0 compile-index)
(operator ":" 2 1 compile-uaddrof)
(operator "_" 2 1 compile-uminus)
(operator "#" 2 1 compile-uderef)
(operator "~" 2 1 compile-unot)
(operator "!" 2 1 compile-ulnot)
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
(operator "<" 6 0 compile-branch)
(operator ">" 6 0 compile-branch)
(operator "<=" 6 0 compile-branch)
(operator ">=" 6 0 compile-branch)
(operator "==" 7 0 compile-branch)
(operator "!=" 7 0 compile-branch)
(operator "&" 8 0 compile-and)
(operator "^" 9 0 compile-xor)
(operator "|" 10 0 compile-or)
(operator "&&" 11 0 compile-land)
(operator "||" 12 0 compile-lor)
(operator "?" 13 0 compile-ternary)
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
		((eq 3 (length b))
			`(opt-find-3 ,i ,r ~b))
		(t (error "Wrong number of args in opt-find" b))))

(defcfun opt-find-1 (i r x)
	(while (and (gt (setq i (dec i)) 0)
				(not (match-list? (defq e (elem i *inst*)) x)))
		(if (or (find r e) (find (elem 0 e) '(vpif elseif else endif))) (setq i 0)))
	(setq m 0 p (if (lt i 1) nil i)))

(defcfun opt-find-2 (i r x y)
	(defq e nil)
	(while (and (gt (setq i (dec i)) 0)
				(not (match-list? (setq m 0 e (elem i *inst*)) x))
				(not (match-list? (setq m 1 e e) y)))
		(if (or (find r e) (find (elem 0 e) '(vpif elseif else endif))) (setq i 0)))
	(setq p (if (lt i 1) nil i)))

(defcfun opt-find-3 (i r x y z)
	(defq e nil)
	(while (and (gt (setq i (dec i)) 0)
				(not (match-list? (setq m 0 e (elem i *inst*)) x))
				(not (match-list? (setq m 1 e e) y))
				(not (match-list? (setq m 2 e e) z)))
		(if (or (find r e) (find (elem 0 e) '(vpif elseif else endif))) (setq i 0)))
	(setq p (if (lt i 1) nil i)))

(defcfun opt-inst-list ()
	(defq i 0 p nil m nil)
	(while (lt (setq i (inc i)) (length *inst*))
		(defq e (elem i *inst*) o (elem 0 e))
		(cond
			((find o '(vp-cpy-ir vp-cpy-ir-b vp-cpy-ir-s vp-cpy-ir-i vp-cpy-ir-ub vp-cpy-ir-us vp-cpy-ir-ui))
				(defq r (elem 1 e))
				(when (opt-find? i r `(vp-lea-i r4 _ ,r) `(vp-lea-i ,r _ ,r))
					(cond
						((eq m 0)
							(elem-set 1 e r4)))
					(elem-set 2 e (elem 2 (elem p *inst*)))
					(elem-set p *inst* '(progn))))
			((defq c (find o '(vp-cpy-ri vp-cpy-ri-b vp-cpy-ri-s vp-cpy-ri-i)))
				(defq r (elem 2 e))
				(when (opt-find? i r `(vp-lea-i r4 _ ,r) `(vp-lea-d ,r _ ,r) `(vp-lea-i ,r _ ,r))
					(cond
						((eq m 0)
							(elem-set 2 e r4))
						((eq m 1)
							(elem-set 0 e (elem c '(vp-cpy-rd vp-cpy-rd-b vp-cpy-rd-s vp-cpy-rd-i)))))
					(elem-set 3 e (elem 2 (elem p *inst*)))
					(elem-set p *inst* '(progn))))
			((eql o 'vp-lea-i)
				(defq r (elem 1 e))
				(when (opt-find? i r `(vp-lea-i ,r _ ,r))
					(elem-set 2 e `(add ,(elem 2 (elem p *inst*)) ,(elem 2 e)))
					(elem-set p *inst* '(progn))))
			((eql o 'vp-lea-d)
				(defq r (elem 2 e))
				(when (opt-find? i r `(vp-cpy-cr _ ,r))
					(elem-set 0 e 'vp-lea-i)
					(elem-set 2 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* '(progn))
					(setq i (dec i))))
			((defq l '(vp-add-cr vp-sub-cr vp-mul-cr
						vp-and-cr vp-or-cr vp-xor-cr
						vp-shl-cr vp-shr-cr vp-asr-cr) c (find o l))
				(defq r (elem 2 e))
				(when (opt-find? i r `(vp-cpy-cr _ ,r) `(,(elem c l) _ ,r))
					(cond
						((eq m 0)
							(setq c (elem c '(add sub mul bit-and bit-or bit-xor bit-shl bit-shr bit-asr)))
							(elem-set 0 e 'vp-cpy-cr))
						((eq m 1)
							(setq c (elem c '(add add mul bit-and bit-or bit-xor add add add)))))
					(elem-set 1 e `(,c ,(elem 1 (elem p *inst*)) ,(elem 1 e)))
					(elem-set p *inst* '(progn))))
			((defq c (find o '(vp-add-rr vp-sub-rr vp-mul-rr
								vp-and-rr vp-or-rr vp-xor-rr
								vp-shl-rr vp-shr-rr vp-asr-rr)))
				(defq r (elem 1 e))
				(when (opt-find? i r `(vp-cpy-cr _ ,r))
					(elem-set 0 e (elem c '(vp-add-cr vp-sub-cr vp-mul-cr
											vp-and-cr vp-or-cr vp-xor-cr
											vp-shl-cr vp-shr-cr vp-asr-cr)))
					(elem-set 1 e (elem 1 (elem p *inst*)))
					(elem-set p *inst* '(progn))
					(setq i (dec i)))))))

;;;;;;;;;;;
; assigment
;;;;;;;;;;;

(defcfun assign-asm-to-script (%1 %2)
	(setq *src* %1 *dst* (map trim (split %2 (code ","))))
	(unless (eq (length *src*) (length *dst*))
		(error "Mismatching number of src/dst paramaters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(setq *inst* (list 'progn))
		(each-rev (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)
			(compile-assign)) *dst*)
		(opt-inst-list)
		(defq i 0)
		(while (lt i (length *src*))
			(res-reg-def (elem i *src*))
			(setq i (inc i)))
		(fill-reg-def)
		(when debug_emit
			(print "(retire " %1 " {" %2 "})")
			(each print *inst*))
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign-script-to-asm (%1 %2)
	(setq *src* (map trim (split %1 (code ","))) *dst* %2)
	(unless (eq (length *src*) (length *dst*))
		(error "Mismatching number of src/dst paramaters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack)
		(setq *inst* (list 'progn))
		(each (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)
			(pop-value)
			(inc-stack)) *src*)
		(opt-inst-list)
		(defq i 0)
		(while (lt i (length *dst*))
			(res-reg-def (elem i *dst*))
			(setq i (inc i)))
		(fill-reg-def)
		(when debug_emit
			(print "(expr {" %1 "} " %2 ")")
			(each print *inst*))
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign-script-to-script (%1 %2)
	(setq *src* (map trim (split %1 (code ","))) *dst* (map trim (split %2 (code ","))))
	(unless (eq (length *src*) (length *dst*))
		(error "Mismatching number of src/dst paramaters !" (list *src* *dst*)))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack)
		(setq *inst* (list 'progn))
		(each (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)) *src*)
		(each-rev (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)
			(compile-assign)) *dst*)
		(opt-inst-list)
		(fill-reg-def)
		(when debug_emit
			(print "(copy {" %1 "} {" %2 "})")
			(each print *inst*))
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign (%1 %2)
	(defq s (str? %1) d (str? %2))
	(cond
		((and s d)
			;script -> script
			(assign-script-to-script %1 %2))
		((and s (not d))
			;script -> asm
			(assign-script-to-asm %1 %2))
		((and (not s) d)
			;asm -> script
			(assign-asm-to-script %1 %2))
		(t
			;asm -> asm
			(setq *src* %1 *dst* %2)
			(map-src-to-dst))))
