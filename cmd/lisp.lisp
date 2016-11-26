;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(defq list (lambda (&rest b) b))

;;;;;;;;;;;;;
; Definitions
;;;;;;;;;;;;;

(defmacro defun (n a &rest b)
	`(defq ,n (lambda ,a ~b)))

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro let (l &rest b)
	`((lambda ,(map (lambda (x) (elem 0 x)) l) ~b) ~(map (lambda (x) (elem 1 x)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro if (x y &rest b)
	(cond ((eq 0 (length b)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~b)))))

(defmacro when (x &rest b)
	`(cond (,x ~b)))

(defmacro unless (x &rest b)
	`(cond ((not ,x) ~b)))

(defmacro until (x &rest b)
	`(while (not ,x) ~b))

(defmacro or (x &rest b)
	(if (eq 0 (length b)) x
		(progn (defq _x (gensym))
			`(progn (defq ,_x ,x)
				(if ,_x ,_x (or ~b))))))

(defmacro and (x &rest b)
	(if (eq 0 (length b)) x
		`(if ,x (and ~b) nil)))

(defmacro for (s e i b)
	(defq _l (gensym) _e (gensym) _i (gensym))
	`(progn (defq ,_l ,s ,_e ,e ,_i ,i)
		(while (lt ,_l ,_e)
			,b
			(setq ,_l (add ,_l ,_i)))))

(defmacro times (c &rest b)
	(defq _c (gensym))
	`(progn (defq ,_c ,c)
		(while (lt 0 ,_c)
	 		(setq ,_c (dec ,_c))
			~b)))

;;;;;;;;;;;;
; Map/Reduce
;;;;;;;;;;;;

(defun min-len (b)
	(defq m (length (elem 0 b)) i 1 e nil)
	(while (lt i (length b))
		(setq e (length (elem i b)) m (if (lt m e) m e) i (inc i)))
	m)

(defun each (f &rest b)
	(defq m (min-len b) e 0 a nil i nil)
	(while (lt e m)
		(setq a (list) i 0)
		(while (lt i (length b))
			(push a (elem e (elem i b)))
			(setq i (inc i)))
		(setq e (inc e) a (apply f a)))
	a)

(defun each-rev (f &rest b)
	(defq e (dec (min-len b)) a nil i nil)
	(while (ge e 0)
		(setq a (list) i 0)
		(while (lt i (length b))
			(push a (elem e (elem i b)))
			(setq i (inc i)))
		(setq e (dec e) a (apply f a)))
	a)

(defun map (f &rest b)
	(defq m (min-len b) l (list) e 0 a nil i nil)
	(while (lt e m)
		(setq a (list) i 0)
		(while (lt i (length b))
			(push a (elem e (elem i b)))
			(setq i (inc i)))
		(push l (apply f a))
		(setq e (inc e)))
	l)

(defun reduce (f l &rest a)
	(if (eq 0 (length a))
		(defq e 1 a (elem 0 l))
		(defq e 0 a (elem 0 a)))
	(while (lt e (length l))
		(setq a (f a (elem e l)) e (inc e)))
	a)

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some-impl (f b)
	(defq m (min-len b) l (list) e 0 a nil v nil i nil)
	(while (and (not v) (lt e m))
		(setq a (list) i 0)
		(while (lt i (length b))
			(push a (elem e (elem i b)))
			(setq i (inc i)))
		(setq v (apply f a) e (inc e)))
	v)

(defun every-impl (f b)
	(defq m (min-len b) l (list) e 0 a nil v t i nil)
	(while (and v (lt e m))
		(setq a (list) i 0)
		(while (lt i (length b))
			(push a (elem e (elem i b)))
			(setq i (inc i)))
		(setq v (apply f a) e (inc e)))
	v)

(defun some (f &rest b) (some-impl f b))
(defun every (f &rest b) (every-impl f b))
(defun notany (f &rest b) (not (some-impl f b)))
(defun notevery (f &rest b) (not (every-impl f b)))

;;;;;;;;;;;;
; Comparison
;;;;;;;;;;;;

(defun equalp (x y)
	(eql (str x) (str y)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defun inc (x)
	(add x 1))

(defun dec (x)
	(sub x 1))

(defun squared (x)
	(mul x x))

(defun cubed (x)
	(mul x x x))

(defun divmod (x y)
	(list (div x y) (mod x y)))

(defun bit-not (x)
	(bit-xor x -1))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun each-line (f b)
	(defq s (file-stream f) l t)
	(while (setq l (read-line s))
		(b l)))

(defun print-file (f)
	(each-line f print))

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun align (x a)
	(bit-and (add x (dec a)) (sub 0 a)))

(defmacro ascii (c)
	(code c))

(defun to-base-char (x)
	(elem x "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun prin-base (x b j)
	(defun prin-b (x j)
		(if (or (ne j 1) (ne 0 (div x b)))
			(prin-b (div x b) (sub j 1)))
		(prin (to-base-char (mod x b))))
	(prin-b x j))

(defun print-env (l e)
	(print "--- " l " ---")
	(each (lambda (x) (if (not (eql (elem 0 x) '*parent*)) (print x))) e))

;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(defun compile-file (*file*)
	(rehash 71)
	(defun import (*file*)
		(when (notany (lambda (x) (eql x *file*)) *imports*)
			(push *imports* *file*)
;			(print "Importing file: " *file*)
			(repl (file-stream *file*))
;			(print "Imported file: " *file*)
			))
	(defmacro equate (&rest b)
		`(def *compile-env* ~b))
	(defmacro defcompilefun (n a &rest b)
;		(if (eql *file* 'inc/vp.inc) (print "Create function: " n))
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defq *imports* (list))
	(defq *emit-buffer* nil *out-buffer* nil)
	(defq *struct* nil *struct-offset* nil *enum* nil *bit* nil)
	(defq *strings* nil *paths* nil *links* nil)
	(defq *switch* nil *switch-nxt* 0 *switch-stk* (list))
	(defq *src* nil *dst* nil)
	(defq *compile-env* (env) *OS* 'Darwin)
	(import *file*)
	(setq *compile-env* nil))

(compile-file 'sys/load_init.vp)
;(compile-file 'test.vp)
