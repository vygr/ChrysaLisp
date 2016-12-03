(import 'inc/vp.inc)

;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

(defcvar '*switch* nil '*switch-nxt* nil '*switch-stk* (list))

(defcfun goto (l)
	(vp-jmp l))

(defcfun gotoif (e l)
	(expr (elem 0 e) '(r0))
	(vp-jmpif '(r0 != 0) l))

(defcfun gotoifnot (e l)
	(expr (elem 0 e) '(r0))
	(vp-jmpifnot '(r0 != 0) l))

(defcfun switch ()
	(push *switch-stk* *switch*)
	(setq *switch* *switch-nxt* *switch-nxt* (inc *switch-nxt*))
	(defcvar (sym (cat "*cnt-" (str *switch*) "*")) 0)
	(vp-label (sym (cat "_begin_" (str *switch*)))))

(defcfun default ()
	(defq s (sym (cat "*cnt-" (str *switch*) "*")) c (eval s))
	(vp-label (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(set s (inc c)))

(defcfun endswitch ()
	(vp-label (sym (cat "_end_" (str *switch*))))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcfun case (e)
	(default)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(progn
			(vp-jmpifnot `(~e) l))))

(defcfun casenot (e)
	(default)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(progn
			(vp-jmpif `(~e) l))))

(defcfun break ()
	(vp-jmp (sym (cat "_end_" (str *switch*)))))

(defcfun breakif (e)
	(defq l (sym (cat "_end_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun breakifnot (e)
	(defq l (sym (cat "_end_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcfun continue ()
	(vp-jmp (sym (cat "_begin_" (str *switch*)))))

(defcfun continueif (e)
	(defq l (sym (cat "_begin_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcfun continueifnot (e)
	(defq l (sym (cat "_begin_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
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
			(print "vp-sub-cr " *var-sp* " " r4))
		(vp-sub-cr *var-sp* r4)
		(setq *var-sp* 0)))

(defcfun pop-scope ()
	(setq *var-sp* (elem 1 (pop *scopes*)))
	(when (ne (length *scopes*) 0)
		(setq *symbols* (slice 0 (elem 0 (elem -2 *scopes*)) *symbols*)))
	(when (ne *var-sp* 0)
		(when debug_emit
			(print "vp-add-cr " *var-sp* " " r4))
		(vp-add-cr *var-sp* r4)
		(setq *var-sp* 0)))

(defcfun unwind ()
	(defq o (get-scope-offset 0))
	(when (ne o 0)
		(when debug_emit
			(print "vp-add-cr " o " " r4))
		(vp-add-cr o r4)))

(defcfun return ()
	(unwind)
	(when debug_emit
		(print "vp-ret"))
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
	(setq %4 (if %4 %4 0) %5 (if %5 %5 ""))
	(defq s (elem 0 (elem -2 *scopes*)))
	(while (ne s (length *symbols*))
		(if (elem 0 (elem s *symbols*))
			(print "Error: Symbol " %1 " redefined !"))
		(setq s (inc s)))
	(push *symbols* (list (length *scopes*) %1 %2 %3 %4 %5)))

(defcfun get-sym (%1)
	;%1 name
	(defq s (dec (length *symbols*)))
	(while (and (ge s 0) (not (eql %1 (elem 1 (elem s *symbols*)))))
		(setq s (dec s))))

(defcfun const (%1 %2)
	;%1 name
	;%2 value
	(def-sym %1 'const %2))

(defcfun operator (%1 %2 &optional %3 %4)
 	;%1 name
	;%2 precidence
	;%3 asociativity
	;%4 compile macro
	(setq %3 (if %3 %3 0) %4 (if %4 %4 'compile_null))
	(def-sym %1 'op (mul %2 2) %3 %4))

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

(defcvar '*struct* nil '*struct-offset* nil
	'null_size 0 'byte_size 1 'short_size 2 'int_size 4 'long_size 8 'ptr_size 8)

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
					(defcvar x *struct-offset*)
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
			(defcvar f *struct-offset*)
			(setq *struct-offset* (add *struct-offset* s)))
		(t
			(def-sym f 'var *var-sp* 0 "")
			(setq *var-sp* (add *var-sp* s)))))

;;;;;;;;;;;;;;;;;;;;
; paramater handling
;;;;;;;;;;;;;;;;;;;;

(defcvar '*src* nil '*dst* nil)

(defcfun set-src (b)
	(setq *src* b))

(defcfun set-dst (b)
	(setq *dst* b))

(defcfun map-remove-ignored ()
	(defq s (list) d (list))
	(each (lambda (x y) (unless (or (eql x y) (eql y '_)) (push s x) (push d y))) *src* *dst*)
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
