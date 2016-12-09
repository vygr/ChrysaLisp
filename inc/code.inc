(import 'inc/vp.inc)

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
	(if (inst-of 'class/class_string e)
		(progn
			(assign e '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(progn
			(vp-jmpifnot `(~e) l))))

(defcfun casenot (e)
	(default)
	(defq c (eval (cnt-sym)) l (loc-sym c))
	(if (inst-of 'class/class_string e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(progn
			(vp-jmpif `(~e) l))))

(defcfun break ()
	(vp-jmp (end-sym)))

(defcfun breakif (e)
	(defq l (end-sym))
	(if (inst-of 'class/class_string e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun breakifnot (e)
	(defq l (end-sym))
	(if (inst-of 'class/class_string e)
		(progn
			(assign e '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcfun continue ()
	(vp-jmp (beg-sym)))

(defcfun continueif (e)
	(defq l (beg-sym))
	(if (inst-of 'class/class_string e)
		(progn
			(assign e '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun continueifnot (e)
	(defq l (beg-sym))
	(if (inst-of 'class/class_string e)
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
		(print "Error: Symbol not in scope !")
		(progn
			(setq s (sub (dec (length *scopes*)) s))
			(while (ne s 0)
				(setq o (add o (elem 1 (elem s *scopes*))) s (dec s)))))
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
			(print "Error: Symbol " %1 " redefined !"))
		(setq s (inc s)))
	(push *symbols* (list (length *scopes*) %1 %2 %3 %4 %5)))

(defcfun get-sym (%1)
	;%1 name
	(defq %1 (sym %1) i (dec (length *symbols*)) s nil)
	(while (and (ge i 0) (not (eql %1 (elem 1 (setq s (elem i *symbols*))))))
		(setq i (dec i)))
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
		(print "Error: Atempt to define offset into local stack !")))

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
	(defq i (inst-of 'class/class_vector d) j (inst-of 'class/class_vector s))
	(cond
		(i (cond
				(j (print "Error: Ilegal src/dst !")
					(print *src* ": " s " -> " *dst* ": " d))
				(t (some (lambda (x) (eql x s)) d))))
		(j (cond
				(i (print "Error: Ilegal src/dst !")
					(print *src* ": " s " -> " *dst* ": " d))
				(t (some (lambda (x) (eql x d)) s))))
		(t (eql s d))))

(defcfun find-later-src (d)
	(defq i -1 j (dec (length *src*)) d (elem d *dst*))
	(while (ne j -1)
		(if (match-any d (elem j *src*))
			(setq i j j -1)
			(setq j (dec j))))
	i)

(defcfun map-rotate (i j)
	(setq *src* (cat (slice 0 i *src*) (slice j (inc j) *src*)
					(slice i j *src*) (slice (inc j) (length *src*) *src*))
		*dst* (cat (slice 0 i *dst*) (slice j (inc j) *dst*)
					(slice i j *dst*) (slice (inc j) (length *dst*) *dst*))))

(defcfun map-topology-sort ()
	(defq i 0 c 1000 j nil)
	(while (and (ne i (length *dst*)) (ne c 0))
		(setq j (find-later-src i))
		(if (gt j i)
			(progn
				(map-rotate i j)
				(setq c (dec c)))
			(setq i (inc i))))
	(when (ne i (length *dst*))
		(print "Error: Copy cycle detected !")
		(print *src* " -> " *dst*)))

(defcfun map-src-to-dst ()
	(when (ne (length *src*) (length *dst*))
		(print "Mismatching number of src/dst parameters !")
		(print *src* " -> " *dst*))
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
					(t (print "Invalid dst paramater !"))))
			((eql i 'c) (vp-cpy-cr x y))
			((eql i 'i) (vp-cpy-ir (elem 0 x) (elem 1 x) y))
			((eql i 'd) (vp-cpy-dr (elem 0 x) (elem 1 x) y))
			((eql i '&i) (vp-lea-i (elem 1 x) (elem 2 x) y))
			((eql i '&d) (vp-lea-d (elem 1 x) (elem 2 x) y))
			((eql i 's) (fn-string x y))
			((eql i '@) (fn-bind (elem 1 x) y))
			((eql i '$) (vp-lea-p (elem 1 x) y))
			(t (print "Invalid src paramater !")))) *src* *dst*))

;;;;;;;;;;;
; tokenizer
;;;;;;;;;;;

(defcvar '*token* nil)

(defcfun push-token (s v)
	(push *token* (list s v)))

(defcfun set-token-list (l)
	(setq *token* (list))
	(defq m -1 u 1 i 0 p "" s nil c nil)
	(while (lt i (length l))
		(setq c (elem i l) s (inc (find {@$"-*~!+.?^</%&|=> )(][} c))) ;"
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
								(setq s (find "-*&~!" p))
								(if (ne s -1)
									(push-token (elem s "_#:~!") m)
									(print "Error: Unknown unary operator " p " !")))
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

(defcfun push-rpn (s v)
	(push *rpn* (list (sym s) v)))

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
							(print "Error: Operator " k " not defined !"))
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

(defcfun print-reg-stack ()
	(defq i 0)
	(while (lt i 16)
		(print (vreg-sym i) " -> " (eval (type-sym i)))
		(setq i (inc i))))

(defcfun reset-reg-stack (&optional %1)
	;%1 = reserved slots
	(setq %1 (if %1 %1 0) *s* 0 *r* 0)
	(while (lt *s* %1)
		(set (type-sym *s*) nil)
		(setq *s* (inc *s*))))

(defcfun res-reg-def (%1)
	;%1 = 'rX'
	(set (vreg-sym *r*) %1 (type-sym *r*) nil)
	(setq *r* (inc *r*)))

(defcfun add-reg-def (%1)
	;%1 = rX
	(defq i 0)
	(while (and (lt i *r*) (not (eql (eval (vreg-sym i)) %1)))
		(setq i (inc i)))
	(when (eq i *r*)
		(set (vreg-sym i) %1)
		(setq *r* (inc *r*))))

(defcfun fill-reg-def ()
	(add-reg-def r0)
	(add-reg-def r1)
	(add-reg-def r2)
	(add-reg-def r3)
	(add-reg-def r5)
	(add-reg-def r6)
	(add-reg-def r7)
	(add-reg-def r8)
	(add-reg-def r9)
	(add-reg-def r10)
	(add-reg-def r11)
	(add-reg-def r12)
	(add-reg-def r13)
	(add-reg-def r14)
	(add-reg-def r15))

(defcfun undef-reg-def ()
	(defq i 0)
	(while (lt i *r*)
		(set (vreg-sym i) nil (type-sym i) nil)
		(setq i (inc i))))

(defcfun pop-type (s)
	(defq u (type-sym s) w (eval u))
	(if w
		(progn
			(defq v (slice 1 -1 w) v (if (eql v "") nil v))
			(set u v)
			(elem 0 w))
		(print "Error: No type info !")))

(defcfun add-inst (i)
	(push *inst* i))

(defcfun pop-value ()
	(setq *s* (dec *s*))
	(compile-deref?)
	(set (type-sym *s*) nil))

(defcfun compile-null ()
	(print "Error: No operator !"))

(defcfun compile-const (%1)
	(set (type-sym *s*) nil)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-cpy-cr ,%1 ,x)))

(defcfun compile-assign ()
	(setq *s* (dec *s*))
	(defq y (vreg-sym *s*) w (pop-type *s*))
	(pop-value)
	(defq x (vreg-sym *s*))
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
					(set (type-sym *s*) nil))
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

(defcfun compile-member ()
	(defq s (dec *s*))
	(setq *s* (sub *s* 2))
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(compile-deref)
	(add-inst `(vp-lea-d ,x ,y ,x))
	(set (type-sym *s*) (eval (type-sym s))))

(defcfun compile-uaddrof ()
	(setq *s* (dec *s*))
	(set (type-sym *s*) nil))

(defcfun compile-field ()
	(defq s (dec *s*))
	(setq *s* (sub *s* 2))
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-lea-d ,x ,y ,x))
	(set (type-sym *s*) (eval (type-sym s))))

(defcfun compile-index ()
	(pop-value)
	(defq s *s*)
	(setq *s* (dec *s*))
	(compile-deref)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-lea-d ,x ,y ,x)))

(defcfun compile-uminus ()
	(pop-value)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-mul-cr -1 ,x)))

(defcfun compile-uderef ()
	(setq *s* (dec *s*))
	(compile-deref)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-lea-i ,x 0 ,x)))

(defcfun compile-unot ()
	(pop-value)
	(defq x (vreg-sym *s*))
	(add-inst `(vp-xor-cr -1 ,x)))

(defcfun compile-mul ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-mul-rr ,y ,x)))

(defcfun compile-divu ()
	(defq s2 *s*)
	(pop-value)
	(defq s1 *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s2) z (vreg-sym s1))
	(add-inst `(vp-ext-rr ,x ,y))
	(add-inst `(vp-div-rrr-u ,z ,y ,x)))

(defcfun compile-remu ()
	(defq s2 *s*)
	(pop-value)
	(defq s1 *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s2) z (vreg-sym s1))
	(add-inst `(vp-ext-rr ,x ,y))
	(add-inst `(vp-div-rrr-u ,z ,y ,x))
	(add-inst `(vp-cpy-rr ,y ,x)))

(defcfun compile-div ()
	(defq s2 *s*)
	(pop-value)
	(defq s1 *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s2) z (vreg-sym s1))
	(add-inst `(vp-ext-rr ,x ,y))
	(add-inst `(vp-div-rrr ,z ,y ,x)))

(defcfun compile-rem ()
	(defq s2 *s*)
	(pop-value)
	(defq s1 *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s2) z (vreg-sym s1))
	(add-inst `(vp-ext-rr ,x ,y))
	(add-inst `(vp-div-rrr ,z ,y ,x))
	(add-inst `(vp-cpy-rr ,y ,x)))

(defcfun compile-plus ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-add-rr ,y ,x)))

(defcfun compile-minus ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-sub-rr ,y ,x)))

(defcfun compile-lshift ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-shl-rr ,y ,x)))

(defcfun compile-rshift ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-shr-rr ,y ,x)))

(defcfun compile-arshift ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-asr-rr ,y ,x)))

(defcfun compile-lt ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '< ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-gt ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '> ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-le ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '<= ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-ge ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '>= ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-eq ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '== ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-ne ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,x '!= ,y)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-and ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-and-rr ,y ,x)))

(defcfun compile-xor ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-xor-rr ,y ,x)))

(defcfun compile-or ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-or-rr ,y ,x)))

(defcfun compile-land ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vpif (list ,y '!= 0)))
	(add-inst `(vpif (list ,x '!= 0)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-lor ()
	(pop-value)
	(defq s *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s))
	(add-inst `(vp-or-rr ,y ,x)))

(defcfun compile-ulnot ()
	(pop-value)
	(defq x (vreg-sym *s*))
	(add-inst `(vpif (list ,x '== 0)))
	(add-inst `(vp-cpy-cr 1 ,x))
	(add-inst '(else))
	(add-inst `(vp-xor-rr ,x ,x))
	(add-inst '(endif)))

(defcfun compile-ternary ()
	(pop-value)
	(defq s2 *s*)
	(pop-value)
	(defq s1 *s*)
	(pop-value)
	(defq x (vreg-sym *s*) y (vreg-sym s1) z (vreg-sym s2))
	(add-inst `(vpif (list ,x '!= 0)))
	(add-inst `(vp-cpy-rr ,y ,x))
	(add-inst '(else))
	(add-inst `(vp-cpy-rr ,z ,x))
	(add-inst '(endif)))

(defcfun compile-operator (%1)
	(defq s (get-sym %1))
	(cond
		((not s)
			(print "Error: Operator " %1 " not defined !"))
		((not (eql 'op (elem 2 s)))
			(print "Error: " %1 " not an operator !"))
		(t
			((elem 5 s)))))

(defcfun compile-string (%1)
	(set (type-sym *s*) nil)
	(defq x (vreg-sym *s*))
	(add-inst `(fn-string ,%1 ,x)))

(defcfun compile-bind (%1)
	(set (type-sym *s*) nil)
	(defq x (vreg-sym *s*))
	(add-inst `(fn-bind ',%1 ,x)))

(defcfun compile-label (%1)
	(set (type-sym *s*) nil)
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
				(print "Error: Unknown token type " k " !")))
		(setq *s* (inc *s*) i (inc i))))

;;;;;;;;;;
; C-Script
;;;;;;;;;;

(defcfun retire (%1 %2)
	(setq *src* %1 *dst* (map trim (split %2 (code ","))))
	(unless (eq (length *src*) (length *dst*))
		(print "Error: Mismatching number of src/dst paramaters !"))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack (length *src*))
		(setq *inst* (list 'progn))
		(each-rev (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)
			(compile-assign)) *dst*)
		;(opt-inst-list)
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

(defcfun expr (%1 %2)
	(setq *src* (map trim (split %1 (code ","))) *dst* %2)
	(unless (eq (length *src*) (length *dst*))
		(print "Error: Mismatching number of src/dst paramaters !"))
	(map-remove-ignored)
	(when (ne 0 (length *dst*))
		(reset-reg-stack)
		(setq *inst* (list 'progn))
		(each (lambda (x)
			(set-token-list x)
			(token-to-rpn)
			(compile-rpn-list)
			(pop-value)
			(setq *s* (inc *s*))) *src*)
		;(opt-inst-list)
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

(defcfun copy (%1 %2)
	(setq *src* (map trim (split %1 (code ","))) *dst* (map trim (split %2 (code ","))))
	(unless (eq (length *src*) (length *dst*))
		(print "Error: Mismatching number of src/dst paramaters !"))
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
		;(opt-inst-list)
		(fill-reg-def)
		(when debug_emit
			(print "(copy {" %1 "} {" %2 "})")
			(each print *inst*))
		(eval *inst*)
		(undef-reg-def)))

(defcfun assign (%1 %2)
	;(print %1 " -> " %2)
	(defq s (inst-of 'class/class_string %1) d (inst-of 'class/class_string %2))
	(cond
		((and s d)
			;script -> script
			(copy %1 %2))
		((and s (not d))
			;script -> asm
			(expr %1 %2))
		((and (not s) d)
			;asm -> script
			(retire %1 %2))
		(t
			;asm -> asm
			(setq *src* %1 *dst* %2)
			(map-src-to-dst))))
