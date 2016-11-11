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

(defun align (x a)
	(bit-and (add x (dec a)) (sub 0 a)))

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

(defun print-env (l e)
	(print "--- " l " ---")
	(each (lambda (x) (if (not (eql (elem 0 x) '*parent*)) (print x))) e))

"------------"
"VP Assembler"
"------------"

"Structures"

(defun align-struct (x)
	(setq *struct-offset* (align *struct-offset* x)))

(defun def-struct (s &optional o)
	(setq *struct* s *struct-offset* (eval (sym (cat (str (if o o "null")) "_size")))))

(defun def-struct-end ()
	(align-struct ptr_size)
	(def *compile-env* (sym (cat (str *struct*) "_size")) *struct-offset*)
	(setq *struct* nil))

(defmacro def-type (n s)
	`(defun ,n (&rest f)
		(each (lambda (x)
			(align-struct ,s)
			(def *compile-env* x *struct-offset*)
			(setq *struct-offset* (add *struct-offset* ,s))) f)))

(defq null_size 0)
(defq byte_size 1)
(defq short_size 2)
(defq int_size 4)
(defq long_size 8)
(defq ptr_size 8)

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

(defun offset (f)
	(def *compile-env* f *struct-offset*))

(defun struct (f s)
	(def *compile-env* f *struct-offset*)
	(setq *struct-offset* (eval (sym (cat (str s) "_size")))))

"Emit Buffer"

(defun emit (&rest b)
	(each (lambda (x)
		(push *emit-buffer* x)) b))

(defun emit-passes ()
	(defq *out-buffer-cnt* 0 *out-buffer-size* 0)
	(while (ne 2 *out-buffer-cnt*)
		(setq *out-buffer* (list))
		(each eval *emit-buffer*)
		(setq *out-buffer-cnt* (if (eq *out-buffer-size* (length *out-buffer*))
			(inc *out-buffer-cnt*)
			(progn (setq *out-buffer-size* (length *out-buffer*)) 0)))))

(defun print-emit-buffer ()
	(defq i 0)
	(while (lt i (length *emit-buffer*))
		(print i " -> " (elem i *emit-buffer*))
		(setq i (inc i))))

(defun print-out-buffer (c)
	(defq i 0)
	(while (lt i (length *out-buffer*))
		(if (eq (mod i c) 0)
			(progn
				(prin-base i 16 4) (prin " : ")))
		(prin-base (elem i *out-buffer*) 16 2) (prin " ")
		(setq i (inc i))
		(if (eq (mod i c) 0)
			(print)))
	(print))

(defun emit-label (s)
	(set s (length *out-buffer*)))

(defun emit-byte (&rest b)
	(each (lambda (x)
		(push *out-buffer* (bit-and x 0xff))) b))

(defun emit-short (&rest b)
	(each (lambda (x)
		(emit-byte x (bit-shr x 8))) b))

(defun emit-int (&rest b)
	(each (lambda (x)
		(emit-short x (bit-shr x 16))) b))

(defun emit-long (&rest b)
	(each (lambda (x)
		(emit-int x (bit-shr x 32))) b))

(defun emit-string (s)
	(each (lambda (x)
		(emit-byte (code x))) s))

(defun emit-align (a &optional b)
	(defq n (align (length *out-buffer*) a))
	(while (ne (length *out-buffer*) n)
		(emit-byte (if b b 0))))

"x64 Emit Functions"

(defun emit-push (&rest b)
	(emit-byte 0x5)
	(each emit-byte b))

(defun emit-pop (&rest b)
	(emit-byte 0x6)
	(each emit-byte b))

(defun emit-rel (x y)
	(emit-byte 0x70 y)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-call-rel (x)
	(emit-byte 0x71)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-jmp-rel (x)
	(emit-byte 0x72)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-cpy-rel (x y)
	(emit-byte 0x73 y)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-cpy-rr (x y)
	(emit-byte 0x0 x y))

(defun emit-cpy-ri (x y z)
	(emit-byte 0x1 x y)
	(emit-int z))

(defun emit-cpy-ir (x y z)
	(emit-byte 0x2 x z)
	(emit-int y))

(defun emit-add-cr (c x)
	(emit-byte 0x11 c x))

(defun emit-add-rr (x y)
	(emit-byte 0x12 x y))

(defun emit-sub-cr (c x)
	(emit-byte 0x21 c x))

(defun emit-sub-rr (x y)
	(emit-byte 0x22 x y))

(defun emit-ret ()
	(emit-byte 0xc3))

"VP Instructions"

(defq r0 0 r1 1 r2 2 r3 3 r4 4 r5 5 r6 6 r7 7 r8 8
	r9 9 r10 10 r11 11 r12 12 r13 3 r14 14 r15 15)

(defun vp-label (s) (emit `(emit-label ',s)) (def *compile-env* s 0))
(defun vp-align (&rest b) (emit `(emit-align ~b)))
(defun vp-string (&rest b) (emit `(emit-string ~b)))
(defun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defun vp-short (&rest b) (emit `(emit-short ~b)))
(defun vp-int (&rest b) (emit `(emit-int ~b)))
(defun vp-long (&rest b) (emit `(emit-long ~b)))
(defun vp-push (&rest b) (emit `(emit-push ~b)))
(defun vp-pop (&rest b) (emit `(emit-pop ~b)))
(defun vp-rel (&rest b) (emit `(emit-rel ~b)))
(defun vp-call-rel (&rest b) (emit `(emit-call-rel ~b)))
(defun vp-jmp-rel (&rest b) (emit `(emit-jmp-rel ~b)))
(defun vp-cpy-rel (&rest b) (emit `(emit-cpy-rel ~b)))
(defun vp-cpy-rr (&rest b) (emit `(emit-cpy-rr ~b)))
(defun vp-cpy-ri (&rest b) (emit `(emit-cpy-ri ~b)))
(defun vp-cpy-ir (&rest b) (emit `(emit-cpy-ir ~b)))
(defun vp-add-cr (&rest b) (emit `(emit-add-cr ~b)))
(defun vp-add-rr (&rest b) (emit `(emit-add-rr ~b)))
(defun vp-sub-cr (&rest b) (emit `(emit-sub-cr ~b)))
(defun vp-sub-rr (&rest b) (emit `(emit-sub-rr ~b)))
(defun vp-ret () (emit `(emit-ret)))

"Functions"

(defun def-func (*func-name* &optional *func-stack*)
	(setq *emit-buffer* (list) *out-buffer* (list))
	(setq *strings* (list) *paths* (list) *links* (list))
	(vp-label '_func_start)
	(vp-long -1)
	(vp-int '(sub _func_end _func_start)
		'(sub _func_entry _func_start)
		'(sub _func_links _func_start)
		'(sub _func_paths _func_start)
		(if *func-stack* *func-stack* 4096))
	(vp-label '_func_name_start)
	(vp-string (str *func-name*))
	(vp-byte 0 '(sub _func_entry _func_name_start))
	(vp-align ptr_size '(sub _func_entry _func_name_start))
	(vp-label '_func_entry))

(defun def-func_end ()
	(defq *cnt* 0)
	(each (lambda (s)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_string")))
		(vp-string s) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *strings*)
	(vp-align ptr_size)
	(vp-label '_func_links)
	(setq *cnt* 0)
	(each (lambda (i)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_link")))
		(vp-long `(sub ,(sym (cat "_ref_" (str i) "_path")) (length *out-buffer*)))
		(setq *cnt* (inc *cnt*))) *links*)
	(vp-long 0)
	(vp-label '_func_paths)
	(setq *cnt* 0)
	(each (lambda (s)
		(vp-label (sym (cat "_ref_" (str *cnt*) "_path")))
		(vp-string (str s)) (vp-byte 0)
		(setq *cnt* (inc *cnt*))) *paths*)
	(vp-align ptr_size)
	(vp-label '_func_end)
	(emit-passes)
	(print-emit-buffer)
	(print-out-buffer 16))

(defmacro def-insert (n l)
	`(defun ,n (s)
		(defq i 0)
		(while (and (lt i (length ,l)) (not (eql s (elem i ,l))))
			(setq i (inc i)))
		(if (eq i (length ,l)) (push ,l s))
		i))

(def-insert fn-add-string *strings*)
(def-insert fn-add-path *paths*)

(defun fn-string (s r)
	(vp-rel (sym (cat "_ref_" (str (fn-add-string s)) "_string")) r))

(defun fn-add-link (p)
	(push *links* (fn-add-path p)))

(defun fn-find-link (p)
	(defq i 0)
	(while (and (lt i (length *links*)) (not (eql p (elem (elem i *links*) *paths*))))
		(setq i (inc i)))
	(if (eq i (length *links*)) (fn-add-link p))
	i)

(defun fn-bind (p r)
	(vp-cpy-rel (sym (cat "_ref_" (str (fn-find-link p)) "_link")) r))

(defun fn-call (p)
	(vp-call-rel (sym (cat "_ref_" (str (fn-find-link p)) "_link"))))

(defun fn-jmp (p)
	(vp-jmp-rel (sym (cat "_ref_" (str (fn-find-link p)) "_link"))))

"Files"

(defun import (*file*)
	(if (notany (lambda (x) (eql x *file*)) *imports*)
		(progn
			(push *imports* *file*)
			(repl (file-stream *file*)))))

(defun compile-file (*file*)
	(defq *imports* (list))
	(defq *emit-buffer* nil *out-buffer* nil)
	(defq *struct* nil *struct-offset* nil)
	(defq *strings* nil *paths* nil *links* nil)
	(defq *compile-env* (env))
	(import *file*)
	(setq *compile-env* nil))

(compile-file "test.vp")
