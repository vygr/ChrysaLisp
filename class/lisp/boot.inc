;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(ffi add "class/num/lisp_add" 0)
(ffi sub "class/num/lisp_sub" 0)
(ffi mul "class/num/lisp_mul" 0)
(ffi div "class/num/lisp_div" 0)
(ffi mod "class/num/lisp_mod" 0)
(ffi min "class/num/lisp_min" 0)
(ffi max "class/num/lisp_max" 0)

(ffi fmul "class/num/lisp_fmul" 0)
(ffi fdiv "class/num/lisp_fdiv" 0)
(ffi fmod "class/num/lisp_fmod" 0)

(ffi shr "class/num/lisp_bshr" 0)
(ffi asr "class/num/lisp_basr" 0)
(ffi shl "class/num/lisp_bshl" 0)
(ffi logand "class/num/lisp_band" 0)
(ffi logior "class/num/lisp_bor" 0)
(ffi logxor "class/num/lisp_bxor" 0)

(ffi eq "class/num/lisp_eq" 0)
(ffi ne "class/num/lisp_ne" 0)
(ffi lt "class/num/lisp_lt" 0)
(ffi gt "class/num/lisp_gt" 0)
(ffi le "class/num/lisp_le" 0)
(ffi ge "class/num/lisp_ge" 0)

(ffi length "class/seq/lisp_length" 0)
(ffi elem "class/seq/lisp_elem" 0)
(ffi find "class/seq/lisp_find" 0)
(ffi slice "class/seq/lisp_slice" 0)
(ffi cat "class/seq/lisp_cat" 0)
(ffi each! "class/seq/lisp_each" 0)
(ffi some! "class/seq/lisp_some" 0)

(ffi clear "class/array/lisp_clear" 0)
(ffi push "class/array/lisp_push" 0)
(ffi pop "class/array/lisp_pop" 0)

(ffi elem-set "class/vector/lisp_elemset" 0)
(ffi merge "class/vector/lisp_merge" 0)
(ffi partition "class/vector/lisp_part" 0)
(ffi match? "class/vector/lisp_match" 0)

(ffi str "class/str/lisp_str" 0)
(ffi split "class/str/lisp_split" 0)
(ffi char "class/str/lisp_char" 0)
(ffi code "class/str/lisp_code" 0)
(ffi cmp "class/str/lisp_cmp" 0)
(ffi save "class/str/lisp_save" 0)
(ffi load "class/str/lisp_load" 0)

(ffi sym "class/sym/lisp_sym" 0)
(ffi gensym "class/sym/lisp_gensym" 0)

(ffi defq "class/hmap/lisp_defq" 1)
(ffi setq "class/hmap/lisp_setq" 1)
(ffi def "class/hmap/lisp_def" 0)
(ffi set "class/hmap/lisp_set" 0)
(ffi def? "class/hmap/lisp_defined" 0)
(ffi undef "class/hmap/lisp_undef" 0)
(ffi env "class/hmap/lisp_env" 0)

(ffi pipe "class/pipe/lisp_create" 0)
(ffi pipe-read "class/pipe/lisp_read" 0)
(ffi pipe-write "class/pipe/lisp_write" 0)

(ffi file-stream "class/stream/lisp_filestream" 0)
(ffi string-stream "class/stream/lisp_strstream" 0)
(ffi read-char "class/stream/lisp_readchar" 0)
(ffi read-line "class/stream/lisp_readline" 0)
(ffi write-char "class/stream/lisp_writechar" 0)
(ffi write "class/stream/lisp_write" 0)

(ffi list "class/vector/lisp_list" 0)
(ffi array "class/array/lisp_array" 0)
(ffi points "class/array/lisp_points" 0)

(ffi type-of "class/lisp/func_type" 0)
(ffi eql "class/lisp/func_eql" 0)
(ffi copy "class/lisp/func_copy" 0)

(ffi macroexpand "class/lisp/func_macroexpand" 0)

(ffi throw "class/lisp/func_throw" 0)
(ffi bind "class/lisp/func_bind" 0)
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
(ffi cond "class/lisp/func_cond" 1)
(ffi while "class/lisp/func_while" 1)
(ffi prin "class/lisp/func_prin" 0)
(ffi print "class/lisp/func_print" 0)

(ffi time "sys/pii/lisp_time" 0)
(ffi age "sys/pii/lisp_age" 0)
(ffi kernel-debug "sys/kernel/lisp_debug" 0)

(defq t 't nil 'nil
	lisp_type_lst (shl 1 0)
	lisp_type_int (shl 1 1)
	lisp_type_str (shl 1 2)
	lisp_type_sym (shl 1 3)
	lisp_type_fnc (shl 1 4)
 	defmacro `(macro (n a &rest _)
		(,list ,defq n (,cat '(macro) (,list a) _))))

(defmacro defun (n a &rest _)
	;(defun name ([arg ...]) body)
	;(list [form ...])
	;(progn [form ...])
	;(array [form ...])
	;(points [form ...])
	;(prin [form ...])
	;(print [form ...])
	;(str [form ...])
	(list (eval defq) n (cat '(lambda) (list a) _)))

(defmacro defun-bind (n a &rest _)
	;(defun-bind name ([arg ...]) body)
	(list (eval defq) n (cat '(lambda) (list a) (bind-fun _))))

(defmacro defmacro-bind (n a &rest _)
	;(defmacro name ([arg ...]) body)
	;(defmacro-bind name ([arg ...]) body)
	(list (eval defq) n (cat '(macro) (list a) (bind-fun _))))

;defined early without use of macro expantions !
(defun bind-fun (_)
	;(bind-fun list) -> list
	(defq _s (list (setq _ (macroexpand _))))
	(while (defq _l (pop _s))
		(each! nil nil nil (lambda (_e)
			(cond
				((cond ((eq (type-of _e) lisp_type_lst) (cond ((ne 0 (length _e))
				(cond ((eql 'quote (elem 0 _e)) nil) (t))))))
					(push _s _e))
				((cond ((eq (type-of _e) lisp_type_sym) (cond ((cond ((eql 'lambda _e) nil)
				((eql 'macro _e) nil) (t)) (cond ((defq _f (def? _e))
				(cond ((eq (type-of _f) lisp_type_fnc) t) (t (cond ((eq (type-of _f) lisp_type_lst)
				(cond ((ne 0 (length _f)) (eql 'lambda (elem 0 _f))))))))))))))
					(elem-set _ _l _f)))) (list _l))) _)
;to disable pre binding uncomment folowing line
;(defun bind-fun (_) _)

;pre bind the previous !
(defq bind-fun (bind-fun bind-fun)
	defun (bind-fun defun) defmacro (bind-fun defmacro)
	defun-bind (bind-fun defun-bind) defmacro-bind (bind-fun defmacro-bind))

(defmacro-bind const (_)
	;(const form)
	(eval _))

(defmacro-bind exec (_)
	;(exec ast)
	`(eval (macroexpand ,_)))

(defmacro-bind lst? (_)
	;(lst? form)
	`(eq (type-of ,_) ,lisp_type_lst))
(defmacro-bind num? (_)
	;(num? form)
	`(eq (type-of ,_) ,lisp_type_int))
(defmacro-bind fnc? (_)
	;(fnc? form)
	`(eq (type-of ,_) ,lisp_type_fnc))
(defmacro-bind str? (_)
	;(str? form)
	`(eq (type-of ,_) ,lisp_type_str))
(defmacro-bind sym? (_)
	;(sym? form)
	`(eq (type-of ,_) ,lisp_type_sym))

(defmacro-bind inc (_)
	;(inc num) -> num
	(list 'add _ 1))
(defmacro-bind dec (_)
	;(dec num) -> num
	(list 'sub _ 1))

(defmacro-bind opt (x y &optional z)
	;(opt var val [cond])
	(cond (z `(if ,x ,z ,y)) (t `(if ,x ,x ,y))))
(defmacro-bind setd (&rest _)
	;(setd var val [var val] ...)
	(defq i -2 l (list 'setq))
	(while (lt (setq i (add i 2)) (length _))
		(push l (elem i _) `(opt ,(elem i _) ,(elem (inc i) _)))) l)

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro-bind let (l &rest _)
	;(let [(var val) ...])
	`((lambda ,(map (lambda (_) (elem 0 _)) l) ~_) ~(map (lambda (_) (elem 1 _)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro-bind if (x y &rest _)
	;(if form form [form])
	(cond ((eq 0 (length _)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~_)))))

(defmacro-bind not (_)
	;(not form)
	`(cond (,_ nil) (t)))

(defmacro-bind when (x &rest _)
	;(when form body)
	`(cond (,x ~_)))

(defmacro-bind unless (x &rest _)
	;(unless form body)
	`(cond (,x nil) (t ~_)))

(defmacro-bind until (x &rest _)
	;(until form body)
	`(while (not ,x) ~_))

(defmacro-bind or (&rest _)
	;(or [form] ...)
	(cond
		((eq (length _) 0) nil)
		((eq (length _) 1) (elem 0 _))
		(t `(if (defq ,(defq _x (gensym)) ,(elem 0 _)) ,_x (or ~(slice 1 -1 _))))))

(defmacro-bind and (&rest _)
	;(and [form] ...)
	(cond
		((eq (length _) 0))
		((eq (length _) 1) (elem 0 _))
		(t `(if ,(elem 0 _) (and ~(slice 1 -1 _))))))

(defmacro-bind case (_k &rest _)
	;(case form [(key body)] ...)
	(if (defq _ (reduce-rev (lambda (_x (_ &rest _y))
			(unless (lst? _) (setq _ (list _)))
			(if (notevery (lambda (_) (eq (type-of _) (const lisp_type_sym))) _)
				(throw "Key not symbol !" _))
			(each! nil nil nil (lambda (_)
				(push (elem 0 _x) _)
				(push (elem 1 _x) (macroexpand (cat '(progn) _y)))) (list _)) _x) _ (list (list) (list)))
			_o (find 'otherwise (elem 0 _)))
		`(eval (elem (or (find ,_k ',(elem 0 _)) ,_o) ',(elem 1 _)))
		`(if (defq ,(defq _i (gensym)) (find ,_k ',(elem 0 _))) (eval (elem ,_i ',(elem 1 _))))))

(defmacro-bind times (c &rest _)
	;(times num body)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (le 0 (setq ,_c (dec ,_c))) ~_)))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defmacro-bind curry (f &rest _)
	;(curry lambda var ...) -> lambda
	`(lambda (&rest _) (apply ,f (cat (list ~_) _))))

(defmacro-bind rcurry (f &rest _)
	;(rcurry lambda var ...) -> lambda
	`(lambda (&rest _) (apply ,f (cat _ (list ~_)))))

(defmacro-bind compose (&rest _)
	;(compose lambda lambda) -> lambda
	`(lambda (_) ,(reduce (lambda (x y)
		(list y x)) _ '_)))

(defun-bind range (b e &optional s)
	;(range start end [step]) -> list
	(defq l (list) s (opt s 1 (abs s)))
	(if (le b e)
		(while (lt b e)
			(push l b)
			(setq b (add b s)))
		(while (gt b e)
			(push l b)
			(setq b (sub b s)))) l)

(defun-bind each-mergeable (_f _l)
	;(each-mergeable lambda seq) -> seq
	(defq _ -1)
	(while (lt (setq _ (inc _)) (length _l))
		(_f (elem _ _l))) _l)

(defun-bind each-mergeable-rev (_f _l)
	;(each-mergeable-rev lambda seq) -> seq
	(each! -1 0 nil _f (list _l)) _l)

(defun-bind each (_f &rest _)
	;(each lambda seq ...)
	(each! nil nil nil _f _))

(defun-bind each-rev (_f &rest _)
	;(each-rev lambda seq ...)
	(each! -1 0 nil _f _))

(defun-bind map (_f &rest _)
	;(map lambda seq ...) -> list
	(defq _l (list))
	(each! nil nil (lambda (_)
		(push _l _)) _f _) _l)

(defun-bind map-rev (_f &rest _)
	;(map-rev lambda seq ...) -> list
	(defq _l (list))
	(each! -1 0 (lambda (_)
		(push _l _)) _f _) _l)

(defun-bind filter (_f _b)
	;(filter lambda seq) -> list
	(defq _l (list))
	(each! nil nil (lambda (_p)
		(if _p (push _l (elem _ _b)))) _f (list _b)) _l)

(defun-bind reduce (_f _l &optional _a)
	;(reduce lambda seq [accum])
	(cond
		(_a (each! nil nil nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))
		(t (setq _a (elem 0 _l))
			(each! 1 nil nil (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))) _a)

(defun-bind reduce-rev (_f _l &optional _a)
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

(defun-bind some (_f &rest _)
	;(some lambda seq ...)
	(some! nil nil t _f _))

(defun-bind every (_f &rest _)
	;(every lambda seq ...)
	(some! nil nil nil _f _))

(defun-bind notany (_f &rest _)
	;(notany lambda seq ...)
	(not (some! nil nil t _f _)))

(defun-bind notevery (_f &rest _)
	;(notevery lambda seq ...)
	(not (some! nil nil nil _f _)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defq min_long (shl -1 63) max_long (shr -1 1)
	min_int (shl -1 31) max_int (shr -1 33))

(defun-bind sign (_)
	;(sign num) -> num
	(cond
		((eq _ 0) 0)
		((lt _ 0) -1)
		(t 1)))

(defun-bind neg (_)
	;(neg num) -> num
	(sub 0 _))

(defmacro-bind minus (_)
	;(minus num) -> num
	(neg _))

(defun-bind abs (_)
	;(abs num) -> num
	(if (lt _ 0) (neg _) _))

(defun-bind lognot (_)
	;(lognot num) -> num
	(logxor _ -1))

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq fp_shift 16 fp_2pi 411774 fp_pi 205887 fp_hpi 102943 fp_qpi 51471 fp_rpi 20861
	fp_int_mask (shl -1 fp_shift) fp_frac_mask (lognot fp_int_mask))

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun-bind ascii-code (_)
	;(ascii-code char) -> num
	(code _))

(defun-bind ascii-char (_)
	;(ascii-char num) -> char
	(char _))

(defmacro-bind starts-with (p _)
	;(starts-with str str)
	`(and (ge (length ,_) ,(length p)) (eql ,p (slice 0 ,(length p) ,_))))

(defmacro-bind ends-with (p _)
	;(ends-with str str)
	`(and (ge (length ,_) ,(length p)) (eql ,p (slice ,(dec (neg (length p))) -1 ,_))))

(defun-bind align (_ _a)
	;(align num pow2) -> num
	(logand (add _ (dec _a)) (neg _a)))

(defun-bind to-base-char (_)
	;(to-base-char num) -> str
	(elem _ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun-bind from-base-char (_)
	;(from-base-char str) -> num
	(setq _ (code _))
	(cond
		((ge _ (ascii-code "a"))
			(sub _ (ascii-code "a") -10))
		((ge _ (ascii-code "A"))
			(sub _ (ascii-code "A") -10))
		(t (sub _ (ascii-code "0")))))

(defun-bind to-num (_)
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

(defun-bind trim-start (s &optional c)
	;(trim-start str [str]) -> str
	(defq c (if c (code c) (ascii-code " ")) i -1)
	(while (and (ne (setq i (inc i)) (length s)) (eql (code s 1 i) c)))
	(slice i -1 s))

(defun-bind trim-end (s &optional c)
	;(trim-end str [str]) -> str
	(defq c (if c (code c) (ascii-code " ")) i (length s))
	(while (and (ne (setq i (dec i)) -1) (eql (code s 1 i) c)))
	(slice 0 (inc i) s))

(defun-bind trim (s &optional c)
	;(trim str [str]) -> str
	(trim-start (trim-end s c) c))

(defun-bind pad (v c &optional f)
	;(pad form width [str]) -> str
	(defq f (opt f " ") v (str v) l (length v) c (sub (max c l) l))
	(while (gt c (length f)) (setq f (cat f f)))
	(cat (slice 0 c f) v))

(defun-bind log2 (_)
	;(log2 num) -> num
	(when (and (not (eq 0 _)) (eq _ (logand _ (neg _))))
		(defq i 0)
		(while (ne 0 (setq _ (shr _ 1)))
			(setq i (inc i))) i))

(defun-bind ntz (_)
	;(ntz num) -> num
	(defq n 64)
	(while (ne _ 0)
		(setq n (dec n) _ (shl _ 1))) n)

(defun-bind nto (_)
	;(nto num) -> num
	(defq n 64 _ (lognot _))
	(while (ne _ 0)
		(setq n (dec n) _ (shl _ 1))) n)

(defun-bind nlz (_)
	;(nlz num) -> num
	(defq n 0)
	(while (gt _ 0)
		(setq n (inc n) _ (shl _ 1))) n)

(defun-bind nlo (_)
	;(nlo num) -> num
	(defq n 0 _ (lognot _))
	(while (gt _ 0)
		(setq n (inc n) _ (shl _ 1))) n)

(defun-bind insert (x &rest y)
	;(insert list form) -> list
	(merge x y))

(defun-bind shuffled (_a &optional _l _h)
	;(shuffled list [start end]) -> list
	(setd _l 0 _h (length _a))
	(shuffle (slice _l _h _a)))

(defun-bind shuffle (_a &optional _l _h)
	;(shuffle list [start end]) -> list
	(setd _l 0 _h (length _a))
	(defq _s (time))
	(defun-bind get-next ()
		(setq _s (abs (logxor 0xa5a5a5a5a5a5a5a5 (mul _s 0x1574937f)))))
	(defun-bind get-val (_)
		(if (gt _ 0) (mod (get-next) _) 0))
	(each! _h _l nil (lambda (x)
		(swap _a _ (get-val (inc _)))) (list _a)) _a)

(defun-bind sorted (_f _a &optional _l _h)
	;(sorted list [start end]) -> list
	(setd _l 0 _h (length _a))
	(sort _f (slice _l _h _a)))

(defun-bind sort (_f _a &optional _l _h)
	;(sort list [start end]) -> list
	(setd _l 0 _h (length _a))
	(defq _q (list _l _h))
	(while (setq _h (pop _q) _l (pop _q))
		(when (lt _l _h)
			(defq _p (partition _f _a _l _h))
			(push (push _q _l _p) (inc _p) _h))) _a)

(defun-bind swap (_ _1 _2)
	;(swap list index index)
	(when (ne _1 _2)
		(defq _t (elem _1 _))
		(elem-set _1 _ (elem _2 _))
		(elem-set _2 _ _t)))

(defmacro-bind get-byte (s o)
	;(get-byte str index) -> num
	(list 'code s 1 o))
(defmacro-bind get-short (s o)
	;(get-short str index) -> num
	(list 'code s 2 o))
(defmacro-bind get-int (s o)
	;(get-int str index) -> num
	(list 'code s 4 o))
(defmacro-bind get-long (s o)
	;(get-long str index) -> num
	(list 'code s 8 o))
(defun-bind get-cstr (s o)
	;(get-cstr str index) -> str
	(defq k o)
	(while (ne 0 (get-byte s o))
		(setq o (inc o)))
	(slice k o s))

(defmacro-bind debug (&rest _)
	;(debug [form ...])
	`(kernel-debug (apply cat (map str (list ~_ ,(ascii-char 10))))))

;;;;;;;;;;;;;;;;;;
; Anaphoric macros
;;;;;;;;;;;;;;;;;;

(defmacro-bind aif (x y &optional z)
	;(aif form form [form])
	`(let ((it ,x))
		(if it ,y ,z)))

(defmacro-bind awhen (x &rest _)
	;(awhen form body)
	`(let ((it ,x))
		(when it ~_)))

(defmacro-bind awhile (x &rest _)
	;(awhile form body)
	`(while (defq ,(defq _x (gensym)) ,x)
		(let ((it ,_x)) ~_)))

(defmacro-bind aand (&rest _)
	;(aand [form] ...)
	(cond
		((eq (length _) 0))
		((eq (length _) 1) (elem 0 _))
		(t `(aif ,(elem 0 _) (aand ~(slice 1 -1 _))))))

(defmacro-bind acond (&rest _)
	;(acond (tst body) ...)
	(reduce (lambda (x _)
		(push x `((defq ,(defq _x (gensym)) ,(elem 0 _)) (let ((it ,_x)) ~(slice 1 -1 _)))))
			_ (list 'cond)))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun-bind each-line (_f _)
	;(each-line lambda path)
	(unless (defq _s (file-stream _))
		(throw "No such file !" _))
	(while (defq _l (read-line _s))
		(_f _l)))

(defun-bind each-pipe-line (_f _p)
	;(each-pipe-line lambda pipe)
	(defq _d "" _v t)
	(while (and _p _v)
		(defq _d (cat _d (pipe-read _p nil)) _i (find (ascii-char 10) _d))
		(when _i
			(defq _i (inc _i) _l (slice 0 _i _d) _d (slice _i -1 _d)
				_v (every _f (split _l (ascii-char 10)))))))

(defmacro-bind import (_)
	;(import path)
	`(unless (find ,_ *file-includes*)
		(push *file-includes* ,_)
		(repl (file-stream ,_) ,_)))

(defun-bind write-line (s _)
	;(write-line stream str) -> str
	(write s _) (write s (ascii-char 10)) _)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Compilation environment
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-bind cpu ()
	;(cpu) -> sym
	(defq o 'x86_64)
	(when (defq f (file-stream 'arch))
		(bind '(o _) (read f 32))) o)

(defun-bind abi ()
	;(abi) -> sym
	(defq o 'AMD64)
	(when (defq f (file-stream 'abi))
		(bind '(o _) (read f 32))) o)

(defq profile nil)

(defun-bind within-compile-env (_)
	;(within-compile-env lambda)
	(defq *compile-env* (env 307) *compile-includes* (list))
	(defmacro-bind defcvar (&rest b)
		`(def *compile-env* ~b))
	(defmacro-bind undefc (&rest b)
		`(undef *compile-env* ~b))
	(defmacro-bind defcfun (n a &rest b)
		`(def *compile-env* ',n (lambda ,a ~b)))
	(defmacro-bind defcmacro (n a &rest b)
		`(def *compile-env* ',n (macro ,a ~b)))
	(defmacro-bind defcfun-bind (n a &rest b)
		`(def *compile-env* ',n (lambda ,a ~(bind-fun b))))
	(defmacro-bind defcmacro-bind (n a &rest b)
		`(def *compile-env* ',n (macro ,a ~(bind-fun b))))
	(defun-bind include (_)
		(unless (find _ *compile-includes*)
			(push *compile-includes* _)
			(unless (defq __ (file-stream _)) (throw "No such file !" _))
			(repl __ _)))
(when profile
	(defmacro-bind defcmacro (n a &rest b)
		`(def *compile-env* ',n (macro ,a (count-call ,(insert-call-count n)) ~b)))
	(defmacro-bind defcfun (n a &rest b)
		`(def *compile-env* ',n (lambda ,a (count-call ,(insert-call-count n)) ~b)))
	(defmacro-bind defcfun-bind (n a &rest b)
		`(def *compile-env* ',n (lambda ,a (count-call ,(insert-call-count n t)) ~(bind-fun b)))))
	(catch (progn (setq _ (_) *compile-env* nil) _) (setq *compile-env* nil)))

(when profile
	(defq call-counts (list))
	(defun-bind insert-call-count (n &optional s)
		(if (defq c (some (lambda (e) (if (eql (elem 0 e) n) _)) call-counts)) c
			(progn
				(push call-counts (list n 0 s))
				(dec (length call-counts)))))
	(defun-bind count-call (_)
		(defq c (elem _ call-counts))
		(elem-set 1 c (inc (elem 1 c))))
	(defmacro-bind print-call-counts ()
		(sort (lambda (x y)
			(if (lt (elem 1 x) (elem 1 y)) -1 0)) call-counts)
		(each print call-counts) nil)
	(defmacro-bind call-test ()
		(compile (all-vp-files))
		(print-call-counts))
	(defmacro-bind defun (n a &rest b)
		`(defq ,n (lambda ,a (count-call ,(insert-call-count n)) ~b)))
	(defmacro-bind defun-bind (n a &rest b)
		`(defq ,n (lambda ,a (count-call ,(insert-call-count n t)) ~(bind-fun b))))
	(defmacro-bind demacro-bind (n a &rest b)
		`(defq ,n (macro ,a (count-call ,(insert-call-count n t)) ~(bind-fun b)))))
