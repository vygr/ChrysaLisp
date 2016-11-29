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

(defun each (_f &rest _b)
	(defq _m (min-len _b) _e 0 _a nil _i nil)
	(while (lt _e _m)
		(setq _a (list) _i 0)
		(while (lt _i (length _b))
			(push _a (elem _e (elem _i _b)))
			(setq _i (inc _i)))
		(setq _e (inc _e) _a (apply _f _a)))
	_a)

(defun each-rev (_f &rest _b)
	(defq _e (dec (min-len _b)) _a nil _i nil)
	(while (ge _e 0)
		(setq _a (list) _i 0)
		(while (lt _i (length _b))
			(push _a (elem _e (elem _i _b)))
			(setq _i (inc _i)))
		(setq _e (dec _e) _a (apply _f _a)))
	_a)

(defun map (_f &rest _b)
	(defq _m (min-len _b) _l (list) _e 0 _a nil _i nil)
	(while (lt _e _m)
		(setq _a (list) _i 0)
		(while (lt _i (length _b))
			(push _a (elem _e (elem _i _b)))
			(setq _i (inc _i)))
		(push _l (apply _f _a))
		(setq _e (inc _e)))
	_l)

(defun reduce (_f _l &rest _a)
	(if (eq 0 (length _a))
		(defq _e 1 _a (elem 0 _l))
		(defq _e 0 _a (elem 0 _a)))
	(while (lt _e (length _l))
		(setq _a (_f _a (elem _e _l)) _e (inc _e)))
	_a)

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some-impl (_f _b)
	(defq _m (min-len _b) _e 0 _a nil _v nil _i nil)
	(while (and (not _v) (lt _e _m))
		(setq _a (list) _i 0)
		(while (lt _i (length _b))
			(push _a (elem _e (elem _i _b)))
			(setq _i (inc _i)))
		(setq _v (apply _f _a) _e (inc _e)))
	_v)

(defun every-impl (_f _b)
	(defq _m (min-len _b) _e 0 _a nil _v t _i nil)
	(while (and _v (lt _e _m))
		(setq _a (list) _i 0)
		(while (lt _i (length _b))
			(push _a (elem _e (elem _i _b)))
			(setq _i (inc _i)))
		(setq _v (apply _f _a) _e (inc _e)))
	_v)

(defun some (_f &rest _b) (some-impl _f _b))
(defun every (_f &rest _b) (every-impl _f _b))
(defun notany (_f &rest _b) (not (some-impl _f _b)))
(defun notevery (_f &rest _b) (not (every-impl _f _b)))

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

(defun each-line (_f _b)
	(defq _s (file-stream _f) _l t)
	(while (setq _l (read-line _s))
		(_b _l)))

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

(defun compile (*file*)
	(rehash 101)
	(defun import (*file*)
		(when (notany (lambda (x) (eql x *file*)) *imports*)
			(push *imports* *file*)
;			(print "Importing file: " *file*)
			(repl (file-stream *file*))
;			(print "Imported file: " *file*)
			))
	(defmacro defcvar (&rest b)
		`(def *compile-env* ~b))
	(defmacro defcfun (n a &rest b)
;		(if (eql *file* 'inc/class.inc) (print "Create function: " n))
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defq *imports* (list))
	(defq *emit-buffer* nil *out-buffer* nil)
	(defq *class* nil *struct* nil *struct-offset* nil *enum* nil *bit* nil)
	(defq *strings* nil *paths* nil *links* nil)
	(defq *switch* nil *switch-nxt* 0 *switch-stk* (list))
	(defq *src* nil *dst* nil)
	(defq *compile-env* (env) *OS* 'Darwin)
	(import *file*)
	(setq *compile-env* nil))

(compile 'class/unordered_set/slice_impl.vp)
;(compile 'test.vp)
