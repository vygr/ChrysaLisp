;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

(ffi = "class/num/lisp_eq" 0)
(ffi /= "class/num/lisp_ne" 0)
(ffi < "class/num/lisp_lt" 0)
(ffi > "class/num/lisp_gt" 0)
(ffi <= "class/num/lisp_le" 0)
(ffi >= "class/num/lisp_ge" 0)

(ffi + "class/num/lisp_add" 0)
(ffi - "class/num/lisp_sub" 0)
(ffi * "class/num/lisp_mul" 0)
(ffi / "class/num/lisp_div" 0)
(ffi % "class/num/lisp_mod" 0)
(ffi min "class/num/lisp_min" 0)
(ffi max "class/num/lisp_max" 0)

(ffi i2r "class/num/lisp_i2r" 0)
(ffi i2f "class/num/lisp_i2f" 0)
(ffi f2i "class/num/lisp_f2i" 0)
(ffi f2r "class/num/lisp_f2r" 0)
(ffi r2i "class/num/lisp_r2i" 0)
(ffi r2f "class/num/lisp_r2f" 0)

(ffi >> "class/num/lisp_shr" 0)
(ffi >>> "class/num/lisp_asr" 0)
(ffi << "class/num/lisp_shl" 0)
(ffi logand "class/num/lisp_and" 0)
(ffi logior "class/num/lisp_or" 0)
(ffi logxor "class/num/lisp_xor" 0)

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
(ffi penv "class/hmap/lisp_parent" 0)

(ffi file-stream "class/stream/lisp_filestream" 0)
(ffi string-stream "class/stream/lisp_strstream" 0)
(ffi read-char "class/stream/lisp_readchar" 0)
(ffi read-line "class/stream/lisp_readline" 0)
(ffi read-avail "class/stream/lisp_readavail" 0)
(ffi write-char "class/stream/lisp_writechar" 0)
(ffi write "class/stream/lisp_write" 0)

(ffi list "class/vector/lisp_list" 0)
(ffi array "class/array/lisp_array" 0)
(ffi nums "class/array/lisp_fixeds" 0)

(ffi type-of "class/lisp/lisp_type" 0)
(ffi eql "class/lisp/lisp_eql" 0)
(ffi copy "class/lisp/lisp_copy" 0)

(ffi macroexpand "class/lisp/lisp_macroexpand" 0)

(ffi throw "class/lisp/lisp_throw" 0)
(ffi bind "class/lisp/lisp_bind" 0)
(ffi read "class/lisp/lisp_read" 0)
(ffi quote "class/lisp/lisp_quote" 1)
(ffi quasi-quote "class/lisp/lisp_qquote" 1)
(ffi eval "class/lisp/lisp_eval" 0)
(ffi lambda "class/lisp/lisp_lambda" 1)
(ffi macro "class/lisp/lisp_lambda" 1)
(ffi progn "class/lisp/lisp_progn" 0)
(ffi apply "class/lisp/lisp_apply" 0)
(ffi repl "class/lisp/lisp_repl" 0)
(ffi catch "class/lisp/lisp_catch" 1)
(ffi cond "class/lisp/lisp_cond" 1)
(ffi while "class/lisp/lisp_while" 1)
(ffi prin "class/lisp/lisp_prin" 0)
(ffi print "class/lisp/lisp_print" 0)
(ffi bind-fun "class/lisp/lisp_bindfun" 0)

(ffi time "sys/pii/lisp_time" 0)
(ffi age "sys/pii/lisp_age" 0)
(ffi random "class/num/lisp_random" 0)

(defq t 't nil 'nil
	lisp_type_lst (<< 1 0)
	lisp_type_int (<< 1 1)
	lisp_type_str (<< 1 2)
	lisp_type_sym (<< 1 3)
	lisp_type_fnc (<< 1 4)
 	defmacro '(macro (n a &rest _) `(defq ,n (macro ,a ~_))))

(defmacro defun (n a &rest _)
	;(defun name ([arg ...]) body)
	;(list [form ...])
	;(progn [form ...])
	;(array [form ...])
	;(nums [form ...])
	;(prin [form ...])
	;(print [form ...])
	;(str [form ...])
	`(defq ,n (lambda ,a ~_)))

(defmacro defun-bind (n a &rest _)
	;(defun-bind name ([arg ...]) body)
	`(defq ,n (lambda ,a ~(bind-fun (macroexpand _)))))

(defmacro defmacro-bind (n a &rest _)
	;(defmacro name ([arg ...]) body)
	;(defmacro-bind name ([arg ...]) body)
	`(defq ,n (macro ,a ~(bind-fun (macroexpand _)))))

(cond
	((def? 'nums)
		;to disable pre binding uncomment this line
		;(defun bind-fun (_) _)
	)(t
		;C++ ChrysaLisp !
		(defun bind-fun (_) _)))

;pre bind the previous !
(each! 0 -1 bind-fun (list (list defun defmacro defun-bind defmacro-bind)))

(defmacro-bind const (_)
	;(const form)
	(eval (macroexpand _)))

(defun-bind exec (_)
	;(exec ast)
	(eval (macroexpand _)))

(defmacro-bind lst? (_)
	;(lst? form) -> bool
	`(= (type-of ,_) ,lisp_type_lst))
(defmacro-bind num? (_)
	;(num? form) -> bool
	`(= (type-of ,_) ,lisp_type_int))
(defmacro-bind fnc? (_)
	;(fnc? form) -> bool
	`(= (type-of ,_) ,lisp_type_fnc))
(defmacro-bind str? (_)
	;(str? form) -> bool
	`(= (type-of ,_) ,lisp_type_str))
(defmacro-bind sym? (_)
	;(sym? form) -> bool
	`(= (type-of ,_) ,lisp_type_sym))

(defmacro-bind inc (_)
	;(inc num) -> num
	`(+ ,_ 1))
(defmacro-bind dec (_)
	;(dec num) -> num
	`(- ,_ 1))

(defmacro-bind opt (x y &optional z)
	;(opt var val [cond])
	(cond (z `(cond (,x ,z) (t ,y))) (t `(cond (,x ,x) (t ,y)))))
(defmacro-bind setd (&rest _)
	;(setd var val [var val] ...)
	(defq i -2 l (list 'setq))
	(while (< (setq i (+ i 2)) (length _))
		(push l (elem i _) (list 'opt (elem i _) (elem (inc i) _)))) l)

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro-bind let (l &rest _)
	;(let ([(var val) ...]) body)
	`((lambda ,(map (lambda (_) (elem 0 _)) l) ~_) ~(map (lambda (_) (elem 1 _)) l)))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro-bind if (x y &optional _)
	;(if tst form [eform])
	(cond (_ `(cond (,x ,y) (t ,_)))
		(t `(cond (,x ,y)))))

(defmacro-bind not (_)
	;(not form) -> t|nil
	`(cond (,_ nil) (t)))

(defmacro-bind when (x &rest _)
	;(when tst body)
	`(cond (,x ~_)))

(defmacro-bind unless (x &rest _)
	;(unless tst body)
	`(cond (,x nil) (t ~_)))

(defmacro-bind until (x &rest _)
	;(until tst body)
	`(while (not ,x) ~_))

(defmacro-bind or (&rest _)
	;(or [tst] ...) -> nil|tst
	(cond
		((= (length _) 0) nil)
		((= (length _) 1) (elem 0 _))
		(t `(if (defq ,(defq _x (gensym)) ,(elem 0 _)) ,_x (or ~(slice 1 -1 _))))))

(defmacro-bind and (&rest _)
	;(and [tst] ...) -> t|nil|tst
	(cond
		((= (length _) 0))
		((= (length _) 1) (elem 0 _))
		(t `(if ,(elem 0 _) (and ~(slice 1 -1 _))))))

(defmacro-bind case (_k &rest _)
	;(case tst [(key body)] ...)
	(if (defq _ (reduce-rev (lambda (_x (_ &rest _y))
			(unless (lst? _) (setq _ (list _)))
			(if (notevery (lambda (_) (= (type-of _) (const lisp_type_sym))) _)
				(throw "Key not symbol !" _))
			(each! 0 -1 (lambda (_)
				(push (elem 0 _x) _)
				(push (elem 1 _x) (macroexpand (cat '(progn) _y)))) (list _)) _x) _ (list (list) (list)))
			_o (or (find t (elem 0 _)) (find 'otherwise (elem 0 _))))
		`(eval (elem (or (find ,_k ',(elem 0 _)) ,_o) ',(elem 1 _)))
		`(if (defq ,(defq _i (gensym)) (find ,_k ',(elem 0 _))) (eval (elem ,_i ',(elem 1 _))))))

(defmacro-bind times (c &rest _)
	;(times num body)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (<= 0 (setq ,_c (dec ,_c))) ~_)))

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
	(if (<= b e)
		(while (< b e)
			(push l b)
			(setq b (+ b s)))
		(while (> b e)
			(push l b)
			(setq b (- b s)))) l)

(defun-bind each-mergeable (_f _l)
	;(each-mergeable lambda seq) -> seq
	(defq _ -1)
	(while (< (setq _ (inc _)) (length _l))
		(_f (elem _ _l))) _l)

(defun-bind each-mergeable-rev (_f _l)
	;(each-mergeable-rev lambda seq) -> seq
	(each! -1 0 _f (list _l)) _l)

(defmacro-bind each (f &rest _)
	;(each lambda seq ...)
	`(each! 0 -1 ,f (list ~_)))

(defmacro-bind each-rev (f &rest _)
	;(each-rev lambda seq ...)
	`(each! -1 0 ,f (list ~_)))

(defun-bind map (_f &rest _)
	;(map lambda seq ...) -> list
	(defq _l (list))
	(each! 0 -1 (lambda (&rest _p)
		(push _l (apply _f _p))) _) _l)

(defun-bind map-rev (_f &rest _)
	;(map-rev lambda seq ...) -> list
	(defq _l (list))
	(each! -1 0 (lambda (&rest _p)
		(push _l (apply _f _p))) _) _l)

(defun-bind filter (_f _b)
	;(filter lambda seq) -> list
	(defq _l (list))
	(each! 0 -1 (lambda (&rest _p)
		(if (apply _f _p) (push _l (elem _ _b)))) (list _b)) _l)

(defun-bind reduce (_f _l &optional _a)
	;(reduce lambda seq [accum]) -> form
	(cond
		(_a (each! 0 -1 (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))
		(t (setq _a (elem 0 _l))
			(each! 1 -1 (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))) _a)

(defun-bind reduce-rev (_f _l &optional _a)
	;(reduce-rev lambda seq [accum]) -> form
	(cond
		(_a (each! -1 0 (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))
		(t (setq _a (elem -2 _l))
			(each! -2 0 (lambda (_p)
				(setq _a (_f _a _p))) (list _l)))) _a)

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun-bind some (_f &rest _)
	;(some lambda seq ...) -> nil|form
	(some! 0 -1 t _f _))

(defun-bind every (_f &rest _)
	;(every lambda seq ...) -> nil|form
	(some! 0 -1 nil _f _))

(defun-bind notany (_f &rest _)
	;(notany lambda seq ...) -> t|nil
	(not (some! 0 -1 t _f _)))

(defun-bind notevery (_f &rest _)
	;(notevery lambda seq ...) -> t|nil
	(not (some! 0 -1 nil _f _)))

;;;;;;;;;;;
; Sequences
;;;;;;;;;;;

(defun-bind erase (s b e)
	;(erase seq start end) -> seq
	(cat (slice 0 b s) (slice e -1 s)))

(defun-bind insert (s p e)
	;(insert seq pos seq) -> seq
	(cat (slice 0 p s) e (slice p -1 s)))

(defun-bind join (l j)
	;(join list seq) -> seq
	(apply cat (map (lambda (x)
		(if (= _ 0) x (cat j x))) l)))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defq min_long (<< -1 63) max_long (>> -1 1) min_int (<< -1 31) max_int (>> -1 33))

(defun-bind sign (_)
	;(sign num) -> -1|0|1
	(cond
		((= _ 0) 0)
		((< _ 0) -1)
		(t 1)))

(defun-bind neg (_)
	;(neg num) -> -num
	(- 0 _))

(defun-bind abs (_)
	;(abs num) -> num
	(if (< _ 0) (neg _) _))

(defun-bind lognot (_)
	;(lognot num) -> num
	(logxor _ -1))

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq fp_shift 16 fp_2pi 411774 fp_pi 205887 fp_hpi 102943 fp_qpi 51471 fp_rpi 20861
	fp_int_mask (<< -1 fp_shift) fp_frac_mask (lognot fp_int_mask))

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defmacro-bind ascii-code (_)
	;(ascii-code char) -> num
	(code _))

(defmacro-bind ascii-char (_)
	;(ascii-char num) -> char
	(char _))

(defun-bind ascii-upper (_)
	;(ascii-upper num) -> num
	(if (<= (ascii-code "a") _ (ascii-code "z"))
		(- _ (const (- (ascii-code "a") (ascii-code "A")))) _))

(defun-bind ascii-lower (_)
	;(ascii-lower num) -> num
	(if (<= (ascii-code "A") _ (ascii-code "Z"))
		(+ _ (const (- (ascii-code "a") (ascii-code "A")))) _))

(defun-bind to-upper (_)
	;(to-upper str) -> str
	(apply cat (map (lambda (_) (char (ascii-upper (code _)))) _)))

(defun-bind to-lower (_)
	;(to-lower str) -> str
	(apply cat (map (lambda (_) (char (ascii-lower (code _)))) _)))

(defmacro-bind starts-with (p _)
	;(starts-with str str) -> t|nil
	`(and (>= (length ,_) ,(length p)) (eql ,p (slice 0 ,(length p) ,_))))

(defmacro-bind ends-with (p _)
	;(ends-with str str) -> t|nil
	`(and (>= (length ,_) ,(length p)) (eql ,p (slice ,(dec (neg (length p))) -1 ,_))))

(defun-bind align (_ _a)
	;(align num pow2) -> num
	(logand (+ _ (dec _a)) (neg _a)))

(defun-bind char-to-num (_)
	;(char-to-num char) -> num
	(cond
		((>= (setq _ (code _)) (ascii-code "a"))
			(- _ (const (- (ascii-code "a") 10))))
		((>= _ (ascii-code "A"))
			(- _ (const (- (ascii-code "A") 10))))
		(t (- _ (ascii-code "0")))))

(defun-bind num-to-char (_)
	;(num-to-char num) -> char
	(elem _ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defun-bind str-to-num (_)
	;(str-to-num str) -> num
	(defq n 0 b 10)
	(when (> (length _) 1)
		(defq i (elem 1 _))
		(cond
			((eql i "x")
				(setq b 16 _ (slice 2 -1 _)))
			((eql i "o")
				(setq b 8 _ (slice 2 -1 _)))
			((eql i "b")
				(setq b 2 _ (slice 2 -1 _)))))
	(defq i -1 f 0)
	(while (< (setq i (inc i)) (length _))
		(defq c (elem i _))
		(if (eql c ".")
			(setq f 1)
			(defq d (char-to-num c) n (+ (* n b) d) f (* f b))))
	(if (= f 0) n (/ (<< n fp_shift) f)))

(defun-bind num-to-utf8 (_)
	(cond
		((>= _ 0x10000)
			(char (+ 0x808080f0 (>> _ 18) (logand (>> _ 4) 0x3f00)
				(logand (<< _ 10) 0x3f0000) (logand (<< _ 24) 0x3f000000)) 4))
		((>= _ 0x800)
			(char (+ 0x8080e0 (>> _ 12) (logand (<< _ 2) 0x3f00)
				(logand (<< _ 16) 0x3f0000)) 3))
		((>= _ 0x80)
			(char (+ 0x80c0 (>> _ 6) (logand (<< _ 8) 0x3f00)) 2))
		(t	(char _))))

(defun-bind trim-start (s &optional c)
	;(trim-start str [str]) -> str
	(defq c (if c (code c) (ascii-code " ")) i -1)
	(while (and (/= (setq i (inc i)) (length s)) (eql (code s 1 i) c)))
	(slice i -1 s))

(defun-bind trim-end (s &optional c)
	;(trim-end str [str]) -> str
	(defq c (if c (code c) (ascii-code " ")) i (length s))
	(while (and (/= (setq i (dec i)) -1) (eql (code s 1 i) c)))
	(slice 0 (inc i) s))

(defun-bind trim (s &optional c)
	;(trim str [str]) -> str
	(trim-start (trim-end s c) c))

(defun-bind pad (v c &optional f)
	;(pad form width [str]) -> str
	(defq f (opt f " ") v (str v) l (length v) c (- (max c l) l))
	(while (> c (length f)) (setq f (cat f f)))
	(cat (slice 0 c f) v))

(defun-bind log2 (_)
	;(log2 num) -> num
	(when (and (not (= 0 _)) (= _ (logand _ (neg _))))
		(defq i 0)
		(while (/= 0 (setq _ (>> _ 1)))
			(setq i (inc i))) i))

(defun-bind ntz (_)
	;(ntz num) -> num
	(defq n 64)
	(while (/= _ 0)
		(setq n (dec n) _ (<< _ 1))) n)

(defun-bind nto (_)
	;(nto num) -> num
	(defq n 64 _ (lognot _))
	(while (/= _ 0)
		(setq n (dec n) _ (<< _ 1))) n)

(defun-bind nlz (_)
	;(nlz num) -> num
	(defq n 0)
	(while (> _ 0)
		(setq n (inc n) _ (<< _ 1))) n)

(defun-bind nlo (_)
	;(nlo num) -> num
	(defq n 0 _ (lognot _))
	(while (> _ 0)
		(setq n (inc n) _ (<< _ 1))) n)

(defun-bind shuffled (_a &optional _l _h)
	;(shuffled list [start end]) -> list
	(setd _l 0 _h (length _a))
	(shuffle (slice _l _h _a)))

(defun-bind shuffle (_a &optional _l _h)
	;(shuffle list [start end]) -> list
	(setd _l 0 _h (length _a))
	(each! _h _l (lambda (x)
		(swap _a _ (random (inc _)))) (list _a)) _a)

(defun-bind sorted (_f _a &optional _l _h)
	;(sorted list [start end]) -> list
	(setd _l 0 _h (length _a))
	(sort _f (slice _l _h _a)))

(defun-bind sort (_f _a &optional _l _h)
	;(sort list [start end]) -> list
	(setd _l 0 _h (length _a))
	(defq _q (list _l _h))
	(while (setq _h (pop _q) _l (pop _q))
		(when (< _l _h)
			(defq _p (partition _f _a _l _h))
			(push (push _q _l _p) (inc _p) _h))) _a)

(defun-bind swap (_ _1 _2)
	;(swap list index index)
	(when (/= _1 _2)
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
	(while (/= 0 (get-byte s o))
		(setq o (inc o)))
	(slice k o s))

(defun-bind type-to-size (_)
	;(type-to-size sym) -> num
	(case _
		((i ui) 4)
		((s us) 2)
		((b ub) 1)
		(t 8)))

;;;;;;;;
; Tuples
;;;;;;;;

(defmacro-bind tuple-set (_i _l _v)
	;(tuple-set index list value)
	(list 'elem-set (eval (macroexpand (eval _i))) _l _v))

(defmacro-bind tuple-get (_i _l)
	;(tuple-get index list) -> value
	(list 'elem (eval (macroexpand (eval _i))) _l))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun-bind each-line (_f _)
	;(each-line lambda stream)
	(while (defq _l (read-line _))
		(_f _l)))

(defmacro-bind import (_)
	;(import path)
	`(unless (find ,_ *file-includes*)
		(push *file-includes* ,_)
		(repl (file-stream ,_) ,_)))

(defmacro-bind read-long (s)
	;(read-long stream) -> num
	(list 'read-char s 8))

(defmacro-bind read-int (s)
	;(read-int stream) -> num
	(list 'read-char s 4))

(defmacro-bind read-short (s)
	;(read-short stream) -> num
	(list 'read-char s 2))

(defmacro-bind write-long (s n)
	;(write-long stream num|list) -> stream
	(list 'write-char s n 8))

(defmacro-bind write-int (s n)
	;(write-int stream num|list) -> stream
	(list 'write-char s n 4))

(defmacro-bind write-short (s n)
	;(write-short stream num|list) -> stream
	(list 'write-char s n 2))

(defun-bind write-line (s _)
	;(write-line stream str) -> stream
	(write-char (write s _) 10))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Compilation environment
;;;;;;;;;;;;;;;;;;;;;;;;;

;compilation options
;debug_mode 0, release, strip all error checking
;debug_mode 1, normal, with error checking
;debug_mode 2, profiling, with error checking and object stats
;debug_mode 3, guarded, with error checking, object stats and guard pages
(defq *debug_mode* 2 *debug_emit* nil *debug_inst* nil)

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
		`(def *compile-env* ',n (lambda ,a ~(bind-fun (macroexpand b)))))
	(defmacro-bind defcmacro-bind (n a &rest b)
		`(def *compile-env* ',n (macro ,a ~(bind-fun (macroexpand b)))))
	(defun-bind include (_)
		(unless (find _ *compile-includes*)
			(push *compile-includes* _)
			(unless (defq __ (file-stream _)) (throw "No such file !" _))
			(repl __ _)))
	(catch (progn (setq _ (_) *compile-env* nil) _) (setq *compile-env* nil)))
