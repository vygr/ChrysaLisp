;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(defq list (lambda (&rest b) b))

(defmacro inc (x) `(add ,x 1))
(defmacro dec (x) `(sub ,x 1))

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
	(defq m (length (elem 0 b)) i 0)
	(while (lt (setq i (inc i)) (length b))
		(defq e (length (elem i b)) m (if (lt m e) m e)))
	m)

(defun each (_f &rest _b)
	(defq _e -1 _a nil)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (lt (setq _e (inc _e)) _m)
				(setq _a (_f (elem _e _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (lt (setq _e (inc _e)) _m)
				(setq _a (_f (elem _e _c) (elem _e _b)))))
		(t
			(defq _m (min-len _b))
			(while (lt (setq _e (inc _e)) _m)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _e (elem _i _b))))
				(setq _a (apply _f _a)))))
	_a)

(defun each-rev (_f &rest _b)
	(defq _a nil)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _e (length _b))
			(while (ge (setq _e (dec _e)) 0)
				(setq _a (_f (elem _e _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _e (min (length _b) (length _c)))
			(while (ge (setq _e (dec _e)) 0)
				(setq _a (_f (elem _e _c) (elem _e _b)))))
		(t
			(defq _e (min-len _b))
			(while (ge (setq _e (dec _e)) 0)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _e (elem _i _b))))
				(setq _a (apply _f _a)))))
	_a)

(defun map (_f &rest _b)
	(defq _l (list) _e -1)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (lt (setq _e (inc _e)) _m)
				(push _l (_f (elem _e _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (lt (setq _e (inc _e)) _m)
				(push _l (_f (elem _e _c) (elem _e _b)))))
		(t
			(defq _m (min-len _b))
			(while (lt (setq _e (inc _e)) _m)
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _e (elem _i _b))))
				(push _l (apply _f _a)))))
	_l)

(defun reduce (_f _l &rest _a)
	(if (eq 0 (length _a))
		(defq _e 0 _a (elem 0 _l))
		(defq _e -1 _a (elem 0 _a)))
	(while (lt (setq _e (inc _e)) (length _l))
		(setq _a (_f _a (elem _e _l))))
	_a)

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some-impl (_f _b)
	(defq _m (min-len _b) _e -1 _v nil)
	(while (and (not _v) (lt (setq _e (inc _e)) _m))
		(defq _a (list) _i -1)
		(while (lt (setq _i (inc _i)) (length _b))
			(push _a (elem _e (elem _i _b))))
		(setq _v (apply _f _a)))
	_v)

(defun every-impl (_f _b)
	(defq _e -1 _v t)
	(cond
		((eq 1 (length _b))
			(defq _b (elem 0 _b) _m (length _b))
			(while (and _v (lt (setq _e (inc _e)) _m))
				(setq _v (_f (elem _e _b)))))
		((eq 2 (length _b))
			(defq _c (elem 0 _b) _b (elem 1 _b) _m (min (length _b) (length _c)))
			(while (and _v (lt (setq _e (inc _e)) _m))
				(setq _v (_f (elem _e _c) (elem _e _b)))))
		(t
			(defq _m (min-len _b))
			(while (and _v (lt (setq _e (inc _e)) _m))
				(defq _a (list) _i -1)
				(while (lt (setq _i (inc _i)) (length _b))
					(push _a (elem _e (elem _i _b))))
				(setq _v (apply _f _a)))))
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

(defmacro minus (x)
	(neg x))

(defun neg (x)
	(sub 0 x))

(defun abs (x)
	(if (lt x 0) (neg x) x))

(defun min (x y)
	(if (lt x y) x y))

(defun max (x y)
	(if (lt x y) y x))

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

(defun from-base-char (c)
	(setq c (code c))
	(cond
		((ge c (ascii "a"))
			(sub c (ascii "a") -10))
		((ge c (ascii "A"))
			(sub c (ascii "A") -10))
		(t
			(sub c (ascii "0")))))

(defun prin-base (x b j)
	(defun prin-b (x j)
		(if (or (ne j 1) (ne 0 (div x b)))
			(prin-b (div x b) (sub j 1)))
		(prin (to-base-char (mod x b))))
	(prin-b x j))

(defun print-env (l e)
	(print "--- " l " ---")
	(each (lambda (x) (if (not (eql (elem 0 x) '*parent*)) (print x))) e)
	t)

(defun find (s c)
	(defq i (length s))
	(while (and (ge (setq i (dec i)) 0) (not (eql c (elem i s)))))
	i)

(defun trim-start (s)
	(while (and (ne 0 (length s)) (eql (elem 0 s) " "))
		(setq s (slice 1 -1 s)))
	s)

(defun trim-end (s)
	(while (and (ne 0 (length s)) (eql (elem -2 s) " "))
		(setq s (slice 0 -2 s)))
	s)

(defun trim (s)
	(trim-start (trim-end s)))

(defun to-num (s)
	(defq n 0 b 10)
	(when (gt (length s) 1)
		(defq i (elem 1 s))
		(cond
			((eql i "x")
				(setq b 16 s (slice 2 -1 s)))
			((eql i "o")
				(setq b 8 s (slice 2 -1 s)))
			((eql i "b")
				(setq b 2 s (slice 2 -1 s)))))
	(defq i -1)
	(while (lt (setq i (inc i)) (length s))
		(setq n (add (mul n b) (from-base-char (elem i s)))))
	n)

(defun match-list? (x y)
	(if (eq (length x) (length y))
		(every (lambda (a b)
			(if (eql b '_) t
				(eql a b))) x y)))

;;;;;;;;;;;;;;
; VP Assembler
;;;;;;;;;;;;;;

(defun platform ()
	(defq o 'Darwin)
	(when (defq f (file-stream 'platform))
		(setq o (sym (read-line f))))
	o)

(defun compile (*file* &optional o)
	(defq *compile-env* (rehash 101) *OS* (if o o (platform)) *imports* (list))
	(defmacro defcvar (&rest b)
		`(def *compile-env* ~b))
	(defmacro defcfun (n a &rest b)
;		(if (eql *file* 'inc/class.inc) (print "Create function: " n))
;		`(def *compile-env* ',n (lambda ,a (print "Enter: " ',n) (defq _rv (progn ~b)) (print "Exit: " ',n) _rv)))
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defun import (*file*)
		(when (notany (lambda (x) (eql x *file*)) *imports*)
			(push *imports* *file*)
			(repl (file-stream *file*))))
	(import *file*)
	(setq *compile-env* nil))

(defun make (&optional d)
	(setq d (if d d ""))
	(compile (sym (cat (str d) "make.inc"))))
