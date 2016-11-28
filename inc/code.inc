(import 'inc/vp.inc)

;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

(defcompilefun goto (l)
	(vp-jmp l))

(defcompilefun gotoif (e l)
	(expr (elem 0 e) '(r0))
	(vp-jmpif '(r0 != 0) l))

(defcompilefun gotoifnot (e l)
	(expr (elem 0 e) '(r0))
	(vp-jmpifnot '(r0 != 0) l))

(defcompilefun switch ()
	(push *switch-stk* *switch*)
	(setq *switch* *switch-nxt* *switch-nxt* (inc *switch-nxt*))
	(equate (sym (cat "*cnt-" (str *switch*) "*")) 0)
	(vp-label (sym (cat "_begin_" (str *switch*)))))

(defcompilefun default ()
	(defq s (sym (cat "*cnt-" (str *switch*) "*")) c (eval s))
	(vp-label (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(set s (inc c)))

(defcompilefun endswitch ()
	(vp-label (sym (cat "_end_" (str *switch*))))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcompilefun case (e)
	(default)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(progn
			(vp-jmpifnot `(~e) l))))

(defcompilefun casenot (e)
	(default)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc_" (str *switch*) "_" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(progn
			(vp-jmpif `(~e) l))))

(defcompilefun break ()
	(vp-jmp (sym (cat "_end_" (str *switch*)))))

(defcompilefun breakif (e)
	(defq l (sym (cat "_end_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcompilefun breakifnot (e)
	(defq l (sym (cat "_end_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcompilefun continue ()
	(vp-jmp (sym (cat "_begin_" (str *switch*)))))

(defcompilefun continueif (e)
	(defq l (sym (cat "_begin_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcompilefun continueifnot (e)
	(defq l (sym (cat "_begin_" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcompilefun loop-start ()
	(switch))

(defcompilefun loop-while (e)
	(switch)
	(breakifnot e))

(defcompilefun loop-whilenot (e)
	(switch)
	(breakif e))

(defcompilefun loop-end ()
	(continue)
	(endswitch))

(defcompilefun loop-until (e)
	(continueifnot e)
	(endswitch))

(defcompilefun loop-untilnot (e)
	(continueif e)
	(endswitch))

(defcompilefun vpif (e)
	(switch)
	(case e))

(defcompilefun vpifnot (e)
	(switch)
	(casenot e))

(defcompilefun elseif (e)
	(break)
	(case e))

(defcompilefun else ()
	(break)
	(default))

(defcompilefun endif ()
	(endswitch))

;;;;;;;
; enums
;;;;;;;

(defcompilefun def-enum (&optional b)
	(setq *enum* (if b b 0)))

(defcompilefun def-enum-end ()
	(setq *enum* nil))

(defcompilefun enum (b)
	(def *compile-env* b *enum*)
	(setq *enum* (inc *enum*)))

;;;;;;
; bits
;;;;;;

(defcompilefun def-bit (&optional b)
	(setq *bit* (if b b 0)))

(defcompilefun def-bit-end ()
	(setq *bit* nil))

(defcompilefun bit (b)
	(def *compile-env* b (bit-shl 1 *bit*))
	(setq *bit* (inc *bit*)))

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

(equate 'null_size 0)
(equate 'byte_size 1)
(equate 'short_size 2)
(equate 'int_size 4)
(equate 'long_size 8)
(equate 'ptr_size 8)

(defcompilefun local-align (&optional x)
	(setq *struct-offset* (align *struct-offset* (if x x long_size))))

(defcompilefun def-struct (s &optional o)
	(setq *struct* s *struct-offset* (eval (sym (cat (str (if o o "null")) "_size")))))

(defcompilefun def-struct-end ()
	(local-align ptr_size)
	(def *compile-env* (sym (cat (str *struct*) "_size")) *struct-offset*)
	(setq *struct* nil))

(defmacro def-type (n s)
	`(defcompilefun ,n (&rest f)
		(each (lambda (x)
			(local-align ,s)
			(def *compile-env* x *struct-offset*)
			(setq *struct-offset* (add *struct-offset* ,s))) f)))

(def-type byte byte_size)
(def-type ubyte byte_size)
(def-type short short_size)
(def-type ushort short_size)
(def-type int int_size)
(def-type uint int_size)
(def-type long long_size)
(def-type ulong long_size)
(def-type ptr ptr_size)
(def-type pbyte ptr_size)
(def-type pubyte ptr_size)
(def-type pshort ptr_size)
(def-type pushort ptr_size)
(def-type pint ptr_size)
(def-type puint ptr_size)
(def-type plong ptr_size)
(def-type pulong ptr_size)
(def-type pptr ptr_size)

(defcompilefun offset (f)
	(def *compile-env* f *struct-offset*))

(defcompilefun struct (f s)
	(def *compile-env* f *struct-offset*)
	(setq *struct-offset* (add *struct-offset* (eval (sym (cat (str s) "_size"))))))

;;;;;;;;;;;;;;;;;;;;
; paramater handling
;;;;;;;;;;;;;;;;;;;;

(defcompilefun set-src (b)
	(setq *src* b))

(defcompilefun set-dst (b)
	(setq *dst* b))

(defcompilefun map-remove-ignored ()
	(defq s (list) d (list))
	(each (lambda (x y) (unless (or (eql x y) (eql y '_)) (push s x) (push d y))) *src* *dst*)
	(setq *src* s *dst* d))

(defcompilefun match-any (d s)
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

(defcompilefun find-later-src (d)
	(defq i -1 j (dec (length *src*)) d (elem d *dst*))
	(while (ne j -1)
		(if (match-any d (elem j *src*))
			(setq i j j -1)
			(setq j (dec j))))
	i)

(defcompilefun map-rotate (i j)
	(setq *src* (cat (slice 0 i *src*) (slice j (inc j) *src*)
					(slice i j *src*) (slice (inc j) (length *src*) *src*))
		*dst* (cat (slice 0 i *dst*) (slice j (inc j) *dst*)
					(slice i j *dst*) (slice (inc j) (length *dst*) *dst*))))

(defcompilefun map-topology-sort ()
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

(defcompilefun map-src-to-dst ()
	(when (ne (length *src*) (length *dst*))
		(print "Mismatching number of src/dst paramaters !"))
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
