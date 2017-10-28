;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(defmacro defun (n a &rest _) `(defq ,n (lambda ,a ~_)))

(defun obj? (_) (inst-of 'class/class_obj _))
(defun lst? (_) (inst-of 'class/class_vector _))
(defun str? (_) (inst-of 'class/class_string _))
(defun sym? (_) (inst-of 'class/class_symbol _))
(defun num? (_) (inst-of 'class/class_boxed_long _))

(defmacro inc (_) `(add ,_ 1))
(defmacro dec (_) `(sub ,_ 1))

(defmacro opt (x y &optional z) (cond (z `(if ,x ,z ,y)) (t `(if ,x ,x ,y))))
(defmacro setd (&rest _)
	(defq i -2 l (list 'setq))
	(while (lt (setq i (add i 2)) (length _))
		(push l (elem i _) `(opt ,(elem i _) ,(elem (inc i) _)))) l)

(defmacro run (_) `(repl (file-stream ,_) ,_))

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro let (l &rest _)
	`((lambda ,(map (lambda (_) (elem 0 _)) l) ~_) ~(map (lambda (_) (elem 1 _)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro if (x y &rest _)
	(cond ((eq 0 (length _)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~_)))))

(defmacro when (x &rest _)
	`(cond (,x ~_)))

(defmacro unless (x &rest _)
	`(cond ((not ,x) ~_)))

(defmacro until (x &rest _)
	`(while (not ,x) ~_))

(defmacro or (x &rest _)
	(if (eq 0 (length _)) x
		`(if (defq ,(defq _x (gensym)) ,x) ,_x (or ~_))))

(defmacro and (x &rest _)
	(if (eq 0 (length _)) x
		`(if ,x (and ~_) nil)))

(defmacro times (c &rest _)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (le 0 (setq ,_c (dec ,_c))) ~_)))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defmacro curry (f &rest _)
	`(lambda (&rest _) (apply ,f (cat (list ~_) _))))

(defmacro rcurry (f &rest _)
	`(lambda (&rest _) (apply ,f (cat _ (list ~_)))))

(defmacro compose (&rest _)
	`(lambda (_) ,(reduce (lambda (x y)
		(list y x)) _ '_)))

(defun range (b e &optional s)
	(defq l (list) s (opt s 1 (abs s)))
	(if (le b e)
		(while (lt b e)
			(push l b)
			(setq b (add b s)))
		(while (gt b e)
			(push l b)
			(setq b (sub b s)))) l)

(defun each-mergeable (_f _l)
	(defq _ -1)
	(while (lt (setq _ (inc _)) (length _l))
		(_f (elem _ _l))))

(defun each-mergeable-rev (_f _l)
	(each! -1 0 nil _f (list _l)))

(defun each (_f &rest _)
	(each! nil nil nil _f _))

(defun each-rev (_f &rest _)
	(each! -1 0 nil _f _))

(defun map (_f &rest _)
	(defq _l (list))
	(each! nil nil (lambda (_)
		(push _l _)) _f _) _l)

(defun map-rev (_f &rest _)
	(defq _l (list))
	(each! -1 0 (lambda (_)
		(push _l _)) _f _) _l)

(defun filter (_f _b)
	(defq _l (list))
	(each! nil nil (lambda (_p)
		(if _p (push _l (elem _ _b)))) _f (list _b)) _l)

(defun reduce (_f _l &optional _a)
	(if _a (defq _ -1)
		(defq _ 0 _a (elem 0 _l)))
	(while (lt (setq _ (inc _)) (length _l))
		(setq _a (_f _a (elem _ _l)))) _a)

(defun reduce-rev (_f _l &optional _a)
	(if _a (defq _ (length _l))
		(defq _ (dec (length _l)) _a (elem -2 _l)))
	(while (ge (setq _ (dec _)) 0)
		(setq _a (_f _a (elem _ _l)))) _a)

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some (_f &rest _)
	(some! nil nil t _f _))

(defun every (_f &rest _)
	(some! nil nil nil _f _))

(defun notany (_f &rest _)
	(not (some! nil nil t _f _)))

(defun notevery (_f &rest _)
	(not (some! nil nil nil _f _)))

;;;;;;;;;;;;
; Comparison
;;;;;;;;;;;;

(defun equalp (x y)
	(eql (str x) (str y)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defq min_long (bit-shl -1 63) max_long (bit-shr -1 1)
	min_int (bit-shl -1 31) max_int (bit-shr -1 33))

(defun sign (_)
	(cond
		((eq _ 0) 0)
		((lt _ 0) -1)
		(t 1)))

(defmacro minus (_)
	(neg _))

(defun neg (_)
	(sub 0 _))

(defun abs (_)
	(if (lt _ 0) (neg _) _))

(defun min (x y)
	(if (lt x y) x y))

(defun max (x y)
	(if (lt x y) y x))

(defun squared (_)
	(mul _ _))

(defun cubed (_)
	(mul _ _ _))

(defun divmod (x y)
	(list (div x y) (mod x y)))

(defun bit-not (_)
	(bit-xor _ -1))

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq fp_shift 16 fp_2pi 411774 fp_pi 205887 fp_hpi 102943 fp_qpi 51471 fp_rpi 20861
	fp_int_mask (bit-shl -1 fp_shift) fp_frac_mask (bit-not fp_int_mask))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun each-line (_ _f)
	(defq _s (file-stream _))
	(while (defq _l (read-line _s))
		(_f _l)))

(defun each-pipe-line (_f _p)
	(defq _d "" _v t)
	(while (and _p _v)
		(defq _d (cat _d (pipe-read _p)) _i (find (char 10) _d))
		(when _i
			(defq _i (inc _i) _l (slice 0 _i _d) _d (slice _i -1 _d)
				_v (every _f (split _l (char 10)))))))

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun align (_ _a)
	(bit-and (add _ (dec _a)) (neg _a)))

(defmacro ascii (_)
	(code _))

(defun to-base-char (_)
	(elem _ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun from-base-char (_)
	(setq _ (code _))
	(cond
		((ge _ (ascii "a"))
			(sub _ (ascii "a") -10))
		((ge _ (ascii "A"))
			(sub _ (ascii "A") -10))
		(t (sub _ (ascii "0")))))

(defun prin-base (x b j)
	(defun prin-b (x j)
		(if (or (ne j 1) (ne 0 (div x b)))
			(prin-b (div x b) (sub j 1)))
		(prin (to-base-char (mod x b))))
	(prin-b x j))

(defun trim-start (s &optional c)
	(setd c " ")
	(while (and (ne 0 (length s)) (eql (elem 0 s) c))
		(setq s (slice 1 -1 s))) s)

(defun trim-end (s &optional c)
	(setd c " ")
	(while (and (ne 0 (length s)) (eql (elem -2 s) c))
		(setq s (slice 0 -2 s))) s)

(defun trim (s &optional c)
	(trim-start (trim-end s c) c))

(defun to-num (_)
	(defq n 0 b 10)
	(when (gt (length _) 1)
		(defq i (elem 1 _))
		(cond
			((eql i "x")
				(setq b 16 _ (slice 2 -1 _)))
			((eql i "o")
				(setq b 8 _ (slice 2 -1 _)))
			((eql i "b")
				(setq b 2 _ (slice 2 -1 _)))))
	(defq i -1 f 0)
	(while (lt (setq i (inc i)) (length _))
		(defq c (elem i _))
		(if (eql c ".")
			(setq f 1)
			(defq d (from-base-char c) n (add (mul n b) d) f (mul f b))))
	(if (eq f 0) n (fdiv n f)))

(defun log2 (_)
	(when (and (not (eq 0 _)) (eq _ (bit-and _ (neg _))))
		(defq i 0)
		(while (ne 0 (setq _ (bit-shr _ 1)))
			(setq i (inc i))) i))

(defun count-trailing-zeros (_)
	(if (eq _ 0) 64)
		(progn
			(defq c 0)
			(while (eq 0 (bit-and _ 1))
				(setq _ (bit-shr _ 1) c (inc c))) c))

(defun count-trailing-ones (_)
	(if (eq _ -1) 64)
		(progn
			(defq c 0)
			(while (ne 0 (bit-and _ 1))
				(setq _ (bit-shr _ 1) c (inc c))) c))

(defun count-leading-ones (_)
	(if (eq _ -1) 64)
		(progn
			(defq c 0)
			(while (gt 0 _)
				(setq _ (bit-shl _ 1) c (inc c))) c))

(defun insert (x y)
	(when (notany (lambda (x) (eql x y)) x)
		(push x y)))

(defun merge (x y)
	(each (lambda (y)
		(when (notany (lambda (x) (eql x y)) x)
			(push x y))) y))

(defun insert-sym (x y)
	(unless (find y x) (push x y)))

(defun merge-sym (x y)
	(each (lambda (y)
		(unless (find y x) (push x y))) y))

(defmacro sym-cat (&rest _)
	`(sym (cat ~_)))

(defun shuffled (_a &optional _l _h)
	(setd _l 0 _h (length _a))
	(shuffle (slice _l _h _a)))

(defun shuffle (_a &optional _l _h)
	(setd _l 0 _h (length _a))
	(defq _s (time))
	(defun get-next ()
		(setq _s (abs (bit-xor 0xa5a5a5a5a5a5a5a5 (mul _s 0x1574937f)))))
	(defun get-int (_)
		(if (gt _ 0) (mod (get-next) _) 0))
	(each! _h _l nil (lambda (x)
		(swap _a _ (get-int (inc _)))) (list _a)) _a)

(defun sorted (_f _a &optional _l _h)
	(setd _l 0 _h (length _a))
	(sort _f (slice _l _h _a)))

(defun sort (_f _a &optional _l _h)
	(setd _l 0 _h (length _a))
	(defq _q (list _l _h))
	(while (setq _h (pop _q) _l (pop _q))
		(when (lt _l _h)
			(defq _p (partition _f _a _l _h))
			(push (push _q _l _p) (inc _p) _h))) _a)

(defun swap (_ _1 _2)
	(when (ne _1 _2)
		(defq _t (elem _1 _))
		(elem-set _1 _ (elem _2 _))
		(elem-set _2 _ _t)))

(defun partition (_f _a _l _h)
	(defq _i _l _p (elem _i _a) _j _l)
	(while (lt (setq _j (inc _j)) _h)
		(when (lt (_f (elem _j _a) _p) 0)
			(swap _a (setq _i (inc _i)) _j)))
	(swap _a _i _l) _i)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Compilation environment
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cpu ()
	(defq o 'x86_64)
	(when (defq f (file-stream 'arch))
		(bind '(o _) (read f 32))) o)

(defun platform ()
	(defq o 'Darwin)
	(when (defq f (file-stream 'platform))
		(bind '(o _) (read f 32))) o)

(defun within-compile-env (_)
	(defq *compile-env* (env 401) *imports* (list))
	(defmacro defcvar (&rest b) `(def *compile-env* ~b))
	(defmacro defcfun (n a &rest b) `(def *compile-env* ',n (lambda ,a ~b)))
	(defmacro defcmacro (n a &rest b) `(def *compile-env* ',n (macro ,a ~b)))
	(defun import (_) (unless (find _ *imports*) (push *imports* _) (run _)))
	(setq _ (_) *compile-env* nil) _)
