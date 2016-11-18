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
	(vp-label (sym (cat "_begin-" (str *switch*)))))

(defcompilefun default ()
	(defq s (sym (cat "*cnt-" (str *switch*) "*")) c (eval s))
	(vp-label (sym (cat "_loc-" (str *switch*) "-" (str c))))
	(set s (inc c)))

(defcompilefun endswitch ()
	(vp-label (sym (cat "_end-" (str *switch*))))
	(default)
	(setq *switch* (pop *switch-stk*)))

(defcompilefun case (e)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc-" (str *switch*) "-" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(progn
			(default)
			(vp-jmpifnot `(~e) l))))

(defcompilefun casenot (e)
	(defq c (eval (sym (cat "*cnt-" (str *switch*) "*")))
		l (sym (cat "_loc-" (str *switch*) "-" (str c))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(progn
			(default)
			(vp-jmpif `(~e) l))))

(defcompilefun break ()
	(vp-jmp (sym (cat "_end-" (str *switch*)))))

(defcompilefun breakif (e)
	(defq l (sym (cat "_end-" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcompilefun breakifnot (e)
	(defq l (sym (cat "_end-" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpifnot '(r0 != 0) l))
		(vp-jmpifnot `(~e) l)))

(defcompilefun continue ()
	(vp-jmp (sym (cat "_begin-" (str *switch*)))))

(defcompilefun continueif (e)
	(defq l (sym (cat "_begin-" (str *switch*))))
	(if (eq 1 (length e))
		(progn
			(expr (elem 0 e) '(r0))
			(vp-jmpif '(r0 != 0) l))
		(vp-jmpif `(~e) l)))

(defcompilefun continueifnot (e)
	(defq l (sym (cat "_begin-" (str *switch*))))
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
	(setq *struct-offset* (eval (sym (cat (str s) "_size")))))

;;;;;;;;;;;;;;;;;;;;
; paramater handling
;;;;;;;;;;;;;;;;;;;;
