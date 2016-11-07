"Primitives"

(defq list (lambda (&rest b) b))

"Definitions"

(defmacro defun (n a &rest b)
	`(defq ,n (lambda ,a ~b)))

"Scopes"

(defmacro let (l &rest b)
	`((lambda ,(map (lambda (x) (elem 0 x)) l) ~b) ~(map (lambda (x) (elem 1 x)) l)))

"Control flow"

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
		(progn
			(defq _x (gensym))
			`(progn
				(defq ,_x ,x)
				(if ,_x ,_x (or ~b))))))

(defmacro and (x &rest b)
	(if (eq 0 (length b)) x
		`(if ,x (and ~b) nil)))

(defmacro for (s e i b)
	(defq _l (gensym) _e (gensym) _i (gensym))
	`(progn
		(defq ,_l ,s ,_e ,e ,_i ,i)
		(while (lt ,_l ,_e)
			,b
			(setq ,_l (add ,_l ,_i)))))

"Map/Reduce"

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

"Predicates"

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

"Comparison"

(defun equalp (x y)
	(eql (str x) (str y)))

"Math functions"

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

"Streams"

(defun each-line (f b)
	(defq s (file-stream f) l t)
	(while (setq l (read-line s))
		(b l)))

(defun print-file (f)
	(each-line f print))

"Utilities"

(defmacro ascii (c)
	(code c))

(defun to-base-char (x)
	(elem x "0123456789abcdefghijklmnopqrstuvwxyz"))

(defun prin-base (x b j)
	(defun prin-b (x j)
		(if (or (ne j 1) (ne 0 (div x b)))
			(prin-b (div x b) (sub j 1)))
		(prin (to-base-char (mod x b))))
	(prin-b x j))

(defun print-map (m)
	(each print m))

(defun print-env (l e)
	(print "**" l "**")
	(each print-map e))

"------------"
"VP Assembler"
"------------"

"Structures"

(defun def-struct (s &optional o)
	(print "structure " s)
	(setq *struct* s *struct-offset* (if o o 0)))

(defun def-struct-end ()
	(print "structure end -> " *struct*))

(defun align-struct (x)
	(setq *struct-offset* (bit-and (add *struct-offset* (sub x 1)) (sub 0 x))))

(defmacro def-type (n s)
	`(defun ,n (&rest f)
		(each (lambda (x)
			(align-struct ,s)
			(defq o *struct-offset*)
			(setq *struct-offset* (add *struct-offset* ,s))
			(print *struct* " -> " x " = " o)) f)))

(defq byte-size 1)
(defq short-size 2)
(defq int-size 4)
(defq long-size 8)
(defq ptr-size 8)

(def-type byte byte-size)
(def-type ubyte byte-size)
(def-type short short-size)
(def-type ushort short-size)
(def-type int int-size)
(def-type uint int-size)
(def-type long long-size)
(def-type ulong long-size)
(def-type ptr ptr-size)
(def-type pbyte ptr-size)
(def-type pubyte ptr-size)
(def-type pshort ptr-size)
(def-type pushort ptr-size)
(def-type pint ptr-size)
(def-type puint ptr-size)
(def-type plong ptr-size)
(def-type pulong ptr-size)
(def-type pptr ptr-size)

(defun offset (f)
	(print *struct* " -> " f " = " *struct-offset*))

"Functions"

(defun emit (&rest b)
	(each (lambda (x) (push *emit-buffer* x)) b))

(defun emit-byte (&rest b)
	(each (lambda (x) (emit (bit-and x 0xff))) b))

(defun emit-short (&rest b)
	(each (lambda (x) (emit-byte x (bit-shr x 8))) b))

(defun emit-int (&rest b)
	(each (lambda (x) (emit-short x (bit-shr x 16))) b))

(defun emit-long (&rest b)
	(each (lambda (x) (emit-int x (bit-shr x 32))) b))

(defun print-emit-buffer (c)
	(defq i 0)
	(while (lt i (length *emit-buffer*))
		(if (eq (mod i c) 0)
			(progn
				(prin-base i 16 4) (prin " : ")))
		(prin-base (elem i *emit-buffer*) 16 2) (prin " ")
		(setq i (inc i))
		(if (eq (mod i c) 0)
			(print)))
	(print))

(defq r0 0 r1 1 r2 2 r3 3 r4 4 r5 5 r6 6 r7 7 r8 8
	r9 9 r10 10 r11 11 r12 12 r13 3 r14 14 r15 15)

(defun vp-add-cr (c x)
	(emit-byte 0x23 x)
	(emit-long c))

(defun vp-add-rr (x y)
	(emit-byte 0x24 x y))

(defun vp-ret ()
	(emit-byte 0xc3))

(defun def-func (n)
	(print "function " n)
	(setq *emit-buffer* (list)))

(defun def-func_end ()
	(print-emit-buffer 16)
	(print "function end"))

"Files"

(defun import (f)
	(if (notany (lambda (x) (eql x f)) *imports*)
		(progn
			(push *imports* f)
			(repl (file-stream f)))))

(defun compile-file (f)
	(defq *imports* (list))
	(defq *struct* "" *struct-offset* 0)
	(defq *emit-buffer* (list))
	(import f))

(compile-file "test.vp")
