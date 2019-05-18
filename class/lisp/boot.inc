;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(ffi add "class/lisp/func_add" 0)
(ffi sub "class/lisp/func_sub" 0)
(ffi mul "class/lisp/func_mul" 0)
(ffi div "class/lisp/func_div" 0)
(ffi mod "class/lisp/func_mod" 0)
(ffi min "class/lisp/func_min" 0)
(ffi max "class/lisp/func_max" 0)

(ffi fmul "class/lisp/func_fmul" 0)
(ffi fdiv "class/lisp/func_fdiv" 0)
(ffi fmod "class/lisp/func_fmod" 0)
(ffi frac "class/lisp/func_frac" 0)
(ffi floor "class/lisp/func_floor" 0)
(ffi fsin "class/lisp/func_fsin" 0)
(ffi fcos "class/lisp/func_fcos" 0)
(ffi fsqrt "class/lisp/func_fsqrt" 0)

(ffi eq "class/lisp/func_eq" 0)
(ffi ne "class/lisp/func_ne" 0)
(ffi lt "class/lisp/func_lt" 0)
(ffi gt "class/lisp/func_gt" 0)
(ffi le "class/lisp/func_le" 0)
(ffi ge "class/lisp/func_ge" 0)

(ffi bit-shr "class/lisp/func_bshr" 0)
(ffi bit-asr "class/lisp/func_basr" 0)
(ffi bit-shl "class/lisp/func_bshl" 0)
(ffi bit-and "class/lisp/func_band" 0)
(ffi bit-or "class/lisp/func_bor" 0)
(ffi bit-xor "class/lisp/func_bxor" 0)

(ffi list "class/lisp/func_list" 0)
(ffi array "class/lisp/func_array" 0)
(ffi points "class/lisp/func_points" 0)
(ffi copy "class/lisp/func_copy" 0)
(ffi clear "class/lisp/func_clear" 0)
(ffi slice "class/lisp/func_slice" 0)
(ffi cat "class/lisp/func_cat" 0)
(ffi elem "class/lisp/func_elem" 0)
(ffi elem-set "class/lisp/func_elemset" 0)
(ffi length "class/lisp/func_length" 0)
(ffi push "class/lisp/func_push" 0)
(ffi pop "class/lisp/func_pop" 0)
(ffi each! "class/lisp/func_each" 0)
(ffi some! "class/lisp/func_some" 0)
(ffi merge-sym "class/lisp/func_merge" 0)
(ffi partition "class/lisp/func_part" 0)

(ffi str "class/lisp/func_str" 0)
(ffi sym "class/lisp/func_sym" 0)
(ffi char "class/lisp/func_char" 0)
(ffi code "class/lisp/func_code" 0)
(ffi find "class/lisp/func_find" 0)
(ffi cmp "class/lisp/func_cmp" 0)
(ffi split "class/lisp/func_split" 0)
(ffi eql "class/lisp/func_eql" 0)
(ffi match? "class/lisp/func_match" 0)
(ffi save "class/lisp/func_save" 0)
(ffi load "class/lisp/func_load" 0)

(ffi defq "class/lisp/func_defq" 1)
(ffi setq "class/lisp/func_setq" 1)
(ffi defmacro "class/lisp/func_defmacro" 1)
(ffi gensym "class/lisp/func_gensym" 0)
(ffi macroexpand "class/lisp/func_macroexpand" 0)
(ffi macroexpand-1 "class/lisp/func_macroexpand_1" 0)
(ffi def "class/lisp/func_def" 0)
(ffi undef "class/lisp/func_undef" 0)
(ffi set "class/lisp/func_set" 0)
(ffi def? "class/lisp/func_defined" 0)
(ffi bind "class/lisp/func_bind" 0)
(ffi env "class/lisp/func_env" 0)
(ffi type? "class/lisp/func_type" 0)

(ffi file-stream "class/lisp/func_filestream" 0)
(ffi string-stream "class/lisp/func_strstream" 0)
(ffi read-char "class/lisp/func_readchar" 0)
(ffi read-line "class/lisp/func_readline" 0)
(ffi write-char "class/lisp/func_writechar" 0)
(ffi write "class/lisp/func_write" 0)
(ffi pipe "class/lisp/func_pipe" 0)
(ffi pipe-read "class/lisp/func_rpipe" 0)
(ffi pipe-write "class/lisp/func_wpipe" 0)
(ffi age "class/lisp/func_age" 0)
(ffi time "class/lisp/func_time" 0)

(ffi read "class/lisp/func_read" 0)
(ffi quote "class/lisp/func_quote" 1)
(ffi quasi-quote "class/lisp/func_qquote" 1)
(ffi eval "class/lisp/func_eval" 0)
(ffi lambda "class/lisp/func_lambda" 1)
(ffi macro "class/lisp/func_lambda" 1)
(ffi progn "class/lisp/func_progn" 0)
(ffi apply "class/lisp/func_apply" 0)
(ffi repl "class/lisp/func_repl" 0)
(ffi catch "class/lisp/func_catch" 1)
(ffi throw "class/lisp/func_error" 0)
(ffi cond "class/lisp/func_cond" 1)
(ffi while "class/lisp/func_while" 1)
(ffi prin "class/lisp/func_prin" 0)
(ffi print "class/lisp/func_print" 0)

(ffi kernel-debug "sys/kernel/lisp_debug" 0)

(defq t 't nil 'nil
	lisp_type_list (bit-shl 1 0)
	lisp_type_integer (bit-shl 1 1)
	lisp_type_string (bit-shl 1 2)
	lisp_type_symbol (bit-shl 1 3))

(defmacro const (_)
	;(const form)
	(eval _))

(defmacro exec (_)
	;(exec ast)
	`(eval (macroexpand ,_)))

(defmacro defun (n a &rest _)
	;(defun name ([arg ...]) body)
	;(list [form ...])
	;(progn [form ...])
	;(array [form ...])
	;(points [form ...])
	;(prin [form ...])
	;(print [form ...])
	`(defq ,n (lambda ,a ~_)))

(defmacro lst? (_)
	;(lst? form)
	`(eq (type? ,_) ,lisp_type_list))
(defmacro num? (_)
	;(num? form)
	`(eq (type? ,_) ,lisp_type_integer))
(defmacro str? (_)
	;(str? form)
	`(eq (type? ,_) ,lisp_type_string))
(defmacro sym? (_)
	;(sym? form)
	`(eq (type? ,_) ,lisp_type_symbol))

(defmacro inc (_)
	;(inc num) -> num
	(list 'add _ 1))
(defmacro dec (_)
	;(dec num) -> num
	(list 'sub _ 1))

(defmacro opt (x y &optional z)
	;(opt var val [cond])
	(cond (z `(if ,x ,z ,y)) (t `(if ,x ,x ,y))))
(defmacro setd (&rest _)
	;(setd var val [var val] ...)
	(defq i -2 l (list 'setq))
	(while (lt (setq i (add i 2)) (length _))
		(push l (elem i _) `(opt ,(elem i _) ,(elem (inc i) _)))) l)

(defmacro debug (&rest _)
	;(debug [form ...])
	`(kernel-debug (apply cat (map str (list ~_ ,(char 10))))))

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro let (l &rest _)
	;(let [(var val) ...])
	`((lambda ,(map (lambda (_) (elem 0 _)) l) ~_) ~(map (lambda (_) (elem 1 _)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro if (x y &rest _)
	;(if form form [form])
	(cond ((eq 0 (length _)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~_)))))

(defmacro not (_)
	;(not form)
	`(cond (,_ nil) (t t)))

(defmacro when (x &rest _)
	;(when form body)
	`(cond (,x ~_)))

(defmacro unless (x &rest _)
	;(unless form body)
	`(cond (,x nil) (t ~_)))

(defmacro until (x &rest _)
	;(until form body)
	`(while (not ,x) ~_))

(defmacro or (&rest _)
	;(or [form] ...)
	(cond
		((eq (length _) 0) nil)
		((eq (length _) 1) (elem 0 _))
		(t `(if (defq ,(defq _x (gensym)) ,(elem 0 _)) ,_x (or ~(slice 1 -1 _))))))

(defmacro and (&rest _)
	;(and [form] ...)
	(cond
		((eq (length _) 0) t)
		((eq (length _) 1) (elem 0 _))
		(t `(if ,(elem 0 _) (and ~(slice 1 -1 _))))))

(defmacro times (c &rest _)
	;(times num body)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (le 0 (setq ,_c (dec ,_c))) ~_)))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defmacro curry (f &rest _)
	;(curry lambda var ...) -> lambda
	`(lambda (&rest _) (apply ,f (cat (list ~_) _))))

(defmacro rcurry (f &rest _)
	;(rcurry lambda var ...) -> lambda
	`(lambda (&rest _) (apply ,f (cat _ (list ~_)))))

(defmacro compose (&rest _)
	;(compose lambda lambda) -> lambda
	`(lambda (_) ,(reduce (lambda (x y)
		(list y x)) _ '_)))

(defun range (b e &optional s)
	;(range start end [step]) -> list
	(defq l (list) s (opt s 1 (abs s)))
	(if (le b e)
		(while (lt b e)
			(push l b)
			(setq b (add b s)))
		(while (gt b e)
			(push l b)
			(setq b (sub b s)))) l)

(defun each-mergeable (_f _l)
	;(each-mergeable lambda seq)
	(defq _ -1)
	(while (lt (setq _ (inc _)) (length _l))
		(_f (elem _ _l))))

(defun each-mergeable-rev (_f _l)
	;(each-mergeable-rev lambda seq)
	(each! -1 0 nil _f (list _l)))

(defun each (_f &rest _)
	;(each lambda seq ...)
	(each! nil nil nil _f _))

(defun each-rev (_f &rest _)
	;(each-rev lambda seq ...)
	(each! -1 0 nil _f _))

(defun map (_f &rest _)
	;(map lambda seq ...) -> list
	(defq _l (list))
	(each! nil nil (lambda (_)
		(push _l _)) _f _) _l)

(defun map-rev (_f &rest _)
	;(map-rev lambda seq ...) -> list
	(defq _l (list))
	(each! -1 0 (lambda (_)
		(push _l _)) _f _) _l)

(defun filter (_f _b)
	;(filter lambda seq) -> seq
	(defq _l (list))
	(each! nil nil (lambda (_p)
		(if _p (push _l (elem _ _b)))) _f (list _b)) _l)

(defun reduce (_f _l &optional _a)
	;(reduce lambda seq [accum])
	(cond
		(_a (each! nil nil nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))
		(t (setq _a (elem 0 _l))
			(each! 1 nil nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))) _a)

(defun reduce-rev (_f _l &optional _a)
	;(reduce-rev lambda seq [accum])
	(cond
		(_a (each! -1 0 nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))
		(t (setq _a (elem -2 _l))
			(each! -2 0 nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))) _a)

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun some (_f &rest _)
	;(some lambda seq ...)
	(some! nil nil t _f _))

(defun every (_f &rest _)
	;(every lambda seq ...)
	(some! nil nil nil _f _))

(defun notany (_f &rest _)
	;(notany lambda seq ...)
	(not (some! nil nil t _f _)))

(defun notevery (_f &rest _)
	;(notevery lambda seq ...)
	(not (some! nil nil nil _f _)))

;;;;;;;;;;;;
; Comparison
;;;;;;;;;;;;

(defun equalp (x y)
	;(equalp form form) -> bool
	(eql (str x) (str y)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defq min_long (bit-shl -1 63) max_long (bit-shr -1 1)
	min_int (bit-shl -1 31) max_int (bit-shr -1 33))

(defun sign (_)
	;(sign num) -> num
	(cond
		((eq _ 0) 0)
		((lt _ 0) -1)
		(t 1)))

(defmacro minus (_)
	;(minus num) -> num
	(neg _))

(defun neg (_)
	;(neg num) -> num
	(sub 0 _))

(defun abs (_)
	;(abs num) -> num
	(if (lt _ 0) (neg _) _))

(defun squared (_)
	;(squared num) -> num
	(mul _ _))

(defun cubed (_)
	;(cubed num) -> num
	(mul _ _ _))

(defun divmod (x y)
	;(divmod num num) -> list
	(list (div x y) (mod x y)))

(defun bit-not (_)
	;(bit-not num) -> num
	(bit-xor _ -1))

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq fp_shift 16 fp_2pi 411774 fp_pi 205887 fp_hpi 102943 fp_qpi 51471 fp_rpi 20861
	fp_int_mask (bit-shl -1 fp_shift) fp_frac_mask (bit-not fp_int_mask))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun each-line (_f _)
	;(each-line lambda path)
	(defq _s (file-stream _))
	(while (defq _l (read-line _s))
		(_f _l)))

(defun each-pipe-line (_f _p)
	;(each-pipe-line lambda pipe)
	(defq _d "" _v t)
	(while (and _p _v)
		(defq _d (cat _d (pipe-read _p nil)) _i (find (const (char 10)) _d))
		(when _i
			(defq _i (inc _i) _l (slice 0 _i _d) _d (slice _i -1 _d)
				_v (every _f (split _l (const (char 10))))))))

(defmacro import (_)
	;(import path)
	`(unless (find ,_ *file-includes*)
		(push *file-includes* ,_)
		(repl (file-stream ,_) ,_)))

(defun write-line (s _)
	;(write-line stream str) -> str
	(write s _) (write s (const (char 10))) _)

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun align (_ _a)
	;(align num pow2) -> num
	(bit-and (add _ (dec _a)) (neg _a)))

(defmacro ascii (_)
	;(ascii str) -> num
	(code _))

(defun to-base-char (_)
	;(to-base-char num) -> str
	(elem _ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun from-base-char (_)
	;(from-base-char str) -> num
	(setq _ (code _))
	(cond
		((ge _ (ascii "a"))
			(sub _ (ascii "a") -10))
		((ge _ (ascii "A"))
			(sub _ (ascii "A") -10))
		(t (sub _ (ascii "0")))))

(defun to-num (_)
	;(to-num str) -> num
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

(defun trim-start (s &optional c)
	;(trim-start str [str]) -> str
	(defq c (if c (code c) (ascii " ")) i -1)
	(while (and (ne (setq i (inc i)) (length s)) (eql (code s 1 i) c)))
	(slice i -1 s))

(defun trim-end (s &optional c)
	;(trim-end str [str]) -> str
	(defq c (if c (code c) (ascii " ")) i (length s))
	(while (and (ne (setq i (dec i)) -1) (eql (code s 1 i) c)))
	(slice 0 (inc i) s))

(defun trim (s &optional c)
	;(trim str [str]) -> str
	(trim-start (trim-end s c) c))

(defun pad (v c &optional f)
	;(pad form width [str]) -> str
	(defq f (opt f " ") v (str v) l (length v) c (sub (max c l) l))
	(while (gt c (length f)) (setq f (cat f f)))
	(cat (slice 0 c f) v))

(defun log2 (_)
	;(log2 num) -> num
	(when (and (not (eq 0 _)) (eq _ (bit-and _ (neg _))))
		(defq i 0)
		(while (ne 0 (setq _ (bit-shr _ 1)))
			(setq i (inc i))) i))

(defun count-trailing-zeros (_)
	;(count-trailing-zeros num) -> num
	(if (eq _ 0) 64)
		(progn
			(defq c 0)
			(while (eq 0 (bit-and _ 1))
				(setq _ (bit-shr _ 1) c (inc c))) c))

(defun count-trailing-ones (_)
	;(count-trailing-ones num) -> num
	(if (eq _ -1) 64)
		(progn
			(defq c 0)
			(while (ne 0 (bit-and _ 1))
				(setq _ (bit-shr _ 1) c (inc c))) c))

(defun count-leading-ones (_)
	;(count-leading-ones num) -> num
	(if (eq _ -1) 64)
		(progn
			(defq c 0)
			(while (gt 0 _)
				(setq _ (bit-shl _ 1) c (inc c))) c))

(defun insert (x y)
	;(insert list str) -> list
	(if (notany (lambda (x) (eql x y)) x)
		(push x y) x))

(defun merge (x y)
	;(merge list str) -> list
	(each (lambda (y)
		(when (notany (lambda (x) (eql x y)) x)
			(push x y))) y) x)

(defun insert-sym (x &rest y)
	;(insert-sym list sym) -> list
	(merge-sym x y))

(defmacro sym-cat (&rest _)
	;(sym-cat str ...) -> sym
	`(sym (cat ~_)))

(defun shuffled (_a &optional _l _h)
	;(shuffled list [start end]) -> list
	(setd _l 0 _h (length _a))
	(shuffle (slice _l _h _a)))

(defun shuffle (_a &optional _l _h)
	;(shuffle list [start end]) -> list
	(setd _l 0 _h (length _a))
	(defq _s (time))
	(defun get-next ()
		(setq _s (abs (bit-xor 0xa5a5a5a5a5a5a5a5 (mul _s 0x1574937f)))))
	(defun get-val (_)
		(if (gt _ 0) (mod (get-next) _) 0))
	(each! _h _l nil (lambda (x)
		(swap _a _ (get-val (inc _)))) (list _a)) _a)

(defun sorted (_f _a &optional _l _h)
	;(sorted list [start end]) -> list
	(setd _l 0 _h (length _a))
	(sort _f (slice _l _h _a)))

(defun sort (_f _a &optional _l _h)
	;(sort list [start end]) -> list
	(setd _l 0 _h (length _a))
	(defq _q (list _l _h))
	(while (setq _h (pop _q) _l (pop _q))
		(when (lt _l _h)
			(defq _p (partition _f _a _l _h))
			(push (push _q _l _p) (inc _p) _h))) _a)

(defun swap (_ _1 _2)
	;(swap list index index)
	(when (ne _1 _2)
		(defq _t (elem _1 _))
		(elem-set _1 _ (elem _2 _))
		(elem-set _2 _ _t)))

(defmacro get-byte (s o)
	;(get-byte str index) -> num
	(list 'code s 1 o))
(defmacro get-short (s o)
	;(get-short str index) -> num
	(list 'code s 2 o))
(defmacro get-int (s o)
	;(get-int str index) -> num
	(list 'code s 4 o))
(defmacro get-long (s o)
	;(get-long str index) -> num
	(list 'code s 8 o))
(defun get-cstr (s o)
	;(get-cstr str index) -> str
	(defq k o)
	(while (ne 0 (get-byte s o))
		(setq o (inc o)))
	(slice k o s))

;;;;;;;;;;;;;;;;;;
; Anaphoric macros
;;;;;;;;;;;;;;;;;;

(defmacro aif (x y &optional z)
	;(aif form form [form])
	`(let ((it ,x))
		(if it ,y ,z)))

(defmacro awhen (x &rest _)
	;(awhen form body)
	`(let ((it ,x))
		(when it ~_)))

(defmacro awhile (x &rest _)
	;(awhile form body)
	`(while (defq ,(defq _x (gensym)) ,x)
		(let ((it ,_x)) ~_)))

(defmacro aand (&rest _)
	;(aand [form] ...)
	(cond
		((eq (length _) 0) t)
		((eq (length _) 1) (elem 0 _))
		(t `(aif ,(elem 0 _) (aand ~(slice 1 -1 _))))))

(defmacro acond (&rest _)
	;(acond (tst body) ...)
	(reduce (lambda (x _)
		(push x `((defq ,(defq _x (gensym)) ,(elem 0 _)) (let ((it ,_x)) ~(slice 1 -1 _)))))
			_ (list 'cond)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Compilation environment
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cpu ()
	;(cpu) -> sym
	(defq o 'x86_64)
	(when (defq f (file-stream 'arch))
		(bind '(o _) (read f 32))) o)

(defun abi ()
	;(abi) -> sym
	(defq o 'AMD64)
	(when (defq f (file-stream 'abi))
		(bind '(o _) (read f 32))) o)

(defun within-compile-env (_)
	;(within-compile-env lambda)
	(defq *compile-env* (env 307) *compile-includes* (list))
	(defmacro defcvar (&rest b)
		`(def *compile-env* ~b))
	(defmacro defcfun (n a &rest b)
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defmacro defcmacro (n a &rest b)
		`(def *compile-env* ',n (macro ,a ~b)))
	(defun include (_)
		(unless (find _ *compile-includes*)
			(push *compile-includes* _)
			(repl (file-stream _) _)))
	(catch (progn (setq _ (_) *compile-env* nil) _) (setq *compile-env* nil)))

;; (defq *cond-cnt* (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) *cond-cnt-i* -1)
;; (defmacro do-cond-cnt ()
;; 	(setq *cond-cnt-i* (inc *cond-cnt-i*))
;; 	`(elem-set ,*cond-cnt-i* *cond-cnt* (inc (elem ,*cond-cnt-i* *cond-cnt*))))
;; (defun print-cond-cnt ()
;; 	(print (slice 0 (inc *cond-cnt-i*) *cond-cnt*)))
