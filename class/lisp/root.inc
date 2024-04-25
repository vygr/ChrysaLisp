;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

;these are build in by the lisp class init function !!!
; (ffi ffi "class/lisp/lisp_ffi" 1)
; (ffi lambda "class/lisp/lisp_lambda" 1)
; (ffi macro "class/lisp/lisp_macro" 1)
; (ffi quote "class/lisp/lisp_quote" 1)
; (ffi quasi-quote "class/lisp/lisp_qquote" 1)

(ffi = "class/num/lisp_eq" 0)
; (= num num ...) -> :t | :nil
(ffi /= "class/num/lisp_ne" 0)
; (/= num num ...) -> :t | :nil
(ffi < "class/num/lisp_lt" 0)
; (< num num ...) -> :t | :nil
(ffi > "class/num/lisp_gt" 0)
; (> num num ...) -> :t | :nil
(ffi <= "class/num/lisp_le" 0)
; (<= num num ...) -> :t | :nil
(ffi >= "class/num/lisp_ge" 0)
; (>= num num ...) -> :t | :nil

(ffi + "class/num/lisp_add" 0)
; (+ num num ...) -> num
(ffi - "class/num/lisp_sub" 0)
; (- num num ...) -> num
(ffi * "class/num/lisp_mul" 0)
; (* num num ...) -> num
(ffi / "class/num/lisp_div" 0)
; (/ num num ...) -> num
(ffi % "class/num/lisp_mod" 0)
; (% num num ...) -> num
(ffi min "class/num/lisp_min" 0)
; (min num num ...) -> num
(ffi max "class/num/lisp_max" 0)
; (max num num ...) -> num
(ffi neg "class/num/lisp_neg" 0)
; (neg num) -> num
(ffi abs "class/num/lisp_abs" 0)
; (abs num) -> num

(ffi n2i "class/num/lisp_n2i" 0)
; (n2i num) -> num
(ffi n2f "class/num/lisp_n2f" 0)
; (n2f num) -> fixed
(ffi n2r "class/num/lisp_n2r" 0)
; (n2r num) -> real
(ffi random "class/num/lisp_random" 0)
; (random num) -> num
(ffi num-intern "class/num/lisp_intern" 0)
; (num-intern num) -> num

(ffi >> "class/num/lisp_shr" 0)
; (>> num cnt) -> num
(ffi >>> "class/num/lisp_asr" 0)
; (>>> num cnt) -> num
(ffi << "class/num/lisp_shl" 0)
; (<< num cnt) -> num
(ffi logand "class/num/lisp_and" 0)
; (logand [num] ...) -> num
(ffi logior "class/num/lisp_or" 0)
; (logior [num] ...) -> num
(ffi logxor "class/num/lisp_xor" 0)
; (logxor [num] ...) -> num

(ffi length "class/seq/lisp_length" 0)
; (length seq) -> num
(ffi elem-get "class/seq/lisp_eget" 0)
; (elem-get seq idx) -> elem
(ffi first "class/seq/lisp_first" 0)
; (first seq) -> :nil | elem
(ffi second "class/seq/lisp_second" 0)
; (second seq) -> :nil | elem
(ffi third "class/seq/lisp_third" 0)
; (third seq) -> :nil | elem
(ffi last "class/seq/lisp_last" 0)
; (last seq) -> :nil | elem
(ffi rest "class/seq/lisp_rest" 0)
; (rest seq) -> empty | seq
(ffi most "class/seq/lisp_most" 0)
; (most seq) -> empty | seq
(ffi find "class/seq/lisp_find" 0)
; (find elem seq [idx]) -> :nil | idx
(ffi find-rev "class/seq/lisp_rfind" 0)
; (find-rev elem seq [idx]) -> :nil | idx
(ffi slice "class/seq/lisp_slice" 0)
; (slice seq start end) -> seq
(ffi partition "class/seq/lisp_partition" 0)
; (partition seq [cnt]) -> (seq ...)
(ffi cat "class/seq/lisp_cat" 0)
; (cat seq ...) -> seq
(ffi each! "class/seq/lisp_each" 0)
; (each! start end lambda seqs)
(ffi some! "class/seq/lisp_some" 0)
; (some! start end mode lambda seqs) -> :nil | val
(ffi map! "class/seq/lisp_map" 0)
; (map! start end lambda seqs [out]) -> out | (...)
(ffi reduce! "class/seq/lisp_reduce" 0)
; (reduce! start end lambda seqs init) -> val

(ffi cap "class/array/lisp_cap" 0)
; (cap len array ...) -> array
(ffi clear "class/array/lisp_clear" 0)
; (clear array ...) -> array
(ffi push "class/array/lisp_push" 0)
; (push array elem ...) -> array
(ffi pop "class/array/lisp_pop" 0)
; (push array elem ...) -> array
(ffi elem-set "class/array/lisp_eset" 0)
; (elem-set array idx elem) -> array

(ffi merge-obj "class/list/lisp_merge" 0)
; (merge-obj dlist slist) -> dlist
(ffi pivot "class/list/lisp_pivot" 0)
; (pivot lambda list start end)
(ffi lmatch? "class/list/lisp_match" 0)
; (lmatch? list list) -> :nil | :t
(ffi copy "class/list/lisp_copy" 0)
; (copy form) -> 'form

(ffi str "class/str/lisp_str" 0)
; (str form) -> str
(ffi str-to-num "class/str/lisp_tonum" 0)
; (str-to-num str) -> num
(ffi expand "class/str/lisp_expand" 0)
; (expand str tab_width) -> str
(ffi char "class/str/lisp_char" 0)
; (char num [width]) -> str
(ffi code "class/str/lisp_code" 0)
; (code str [width index]) -> num
(ffi cmp "class/str/lisp_cmp" 0)
; (cmp str str) -> + | 0 | -
(ffi save "class/str/lisp_save" 0)
; (save str path) -> str
(ffi load "class/str/lisp_load" 0)
; (load path) -> str
(ffi str-alloc "class/str/lisp_create" 0)
; (str-alloc size) -> str
(ffi bfind "class/str/lisp_bfind" 0)
; (bfind char cls) -> :nil | idx
(ffi bskip "class/str/lisp_bskip" 0)
; (bskip cls str idx) -> idx
(ffi bskipn "class/str/lisp_bskipn" 0)
; (bskipn cls str idx) -> idx

(ffi sym "class/sym/lisp_sym" 0)
; (sym str) -> sym
(ffi gensym "class/sym/lisp_gensym" 0)
; (gensym) -> sym

(ffi defq "class/hmap/lisp_defq" 1)
; (defq sym val [sym val] ...) -> val
(ffi setq "class/hmap/lisp_setq" 1)
; (setq sym val [sym val] ...) -> val
(ffi def "class/hmap/lisp_def" 0)
; (def env sym val [sym val] ...) -> val
(ffi set "class/hmap/lisp_set" 0)
; (set env sym val [sym val] ...) -> val
(ffi get "class/hmap/lisp_get" 0)
; (get sym [env]) -> :nil | val
(ffi def? "class/hmap/lisp_defx" 0)
; (def? sym [env]) -> :nil | val
(ffi undef "class/hmap/lisp_undef" 0)
; (undef env sym [sym] ...) -> env
(ffi env "class/hmap/lisp_env" 0)
; (env [num]) -> env
(ffi penv "class/hmap/lisp_penv" 0)
; (penv [env]) -> :nil | env
(ffi env-resize "class/hmap/lisp_resize" 0)
; (env-resize num [env]) -> env
(ffi tolist "class/hmap/lisp_tolist" 0)
; (tolist env) -> ((sym val) ...)

(ffi io-stream "class/stream/lisp_iostream" 0)
; (io-stream io) -> :nil | stream
(ffi string-stream "class/stream/lisp_sstream" 0)
; (string-stream str) -> stream
(ffi file-stream "class/stream/lisp_fstream" 0)
; (file-stream path [mode]) -> :nil | stream
(ffi read-char "class/stream/lisp_readchar" 0)
; (read-char stream [width]) -> :nil | str
(ffi read-line "class/stream/lisp_readline" 0)
; (read-line stream) -> :nil | str
(ffi read-avail "class/stream/lisp_readavail" 0)
; (read-avail stream) -> :nil | num
(ffi write-char "class/stream/lisp_writechar" 0)
; (write-char stream list|num [width]) -> stream
(ffi write "class/stream/lisp_write" 0)
; (write stream str) -> stream

(ffi array "class/array/lisp_array" 0)
; (array [num ...]) -> array
(ffi list "class/list/lisp_list" 0)
; (list [elem] ...) -> list
(ffi nums "class/array/lisp_nums" 0)
; (nums [num ...]) -> nums
(ffi fixeds "class/array/lisp_fixeds" 0)
; (fixeds [fixed ...]) -> fixeds
(ffi reals "class/array/lisp_reals" 0)
; (reals [real ...]) -> reals
(ffi path "class/array/lisp_path" 0)
; (path [fixed ...]) -> path
(ffi dim "class/dim/lisp_dim" 0)
; (dim nums array) -> dim
(ffi dim-get "class/dim/lisp_dget" 0)
; (dim-get dim nums) -> elem
(ffi dim-set "class/dim/lisp_dset" 0)
; (dim-set dim nums elem) -> array

(ffi hash "class/obj/lisp_hash" 0)
; (hash form) -> num
(ffi eql "class/lisp/lisp_eql" 0)
; (eql form form) -> :nil | :t
(ffi type-of "class/obj/lisp_type" 0)
; (type-of form) -> (... :obj)
(ffi macroexpand "class/lisp/lisp_macroexpand" 0)
; (macroexpand form) -> 'form

(ffi progn "class/lisp/lisp_progn" 1)
; (progn [body]) -> 'form
(ffi catch "class/lisp/lisp_catch" 1)
; (catch form eform) -> 'form
(ffi cond "class/lisp/lisp_cond" 1)
; (cond [(tst body)] ...) -> 'form
(ffi if "class/lisp/lisp_if" 1)
; (if tst form [else_form]) -> 'form
(ffi while "class/lisp/lisp_while" 1)
; (while tst [body]) -> :nil
(ffi throw "class/lisp/lisp_throw" 0)
; (throw str form)
(ffi bind "class/lisp/lisp_bind" 0)
; (bind (sym ...) seq) -> val
(ffi read "class/lisp/lisp_read" 0)
; (read stream [last_char]) -> :nil | form
(ffi eval "class/lisp/lisp_eval" 0)
; (eval form [env]) -> 'form
(ffi identity "class/lisp/lisp_identity" 0)
; (identity [form]) -> :nil | form
(ffi apply "class/lisp/lisp_apply" 0)
; (apply lambda seq) -> form
(ffi repl "class/lisp/lisp_repl" 0)
; (repl stream name) -> form
(ffi prin "class/lisp/lisp_prin" 0)
; (prin [form] ...) -> form
(ffi print "class/lisp/lisp_print" 0)
; (print [form] ...) -> form
(ffi prebind "class/lisp/lisp_prebind" 0)
; (prebind form) -> form
(ffi . "class/lisp/lisp_mcall" 0)
; (. env sym [...]) -> form
(ffi env-push "class/lisp/lisp_env_push" 0)
; (env-push [env]) -> 'env
(ffi env-pop "class/lisp/lisp_env_pop" 0)
; (env-pop [env]) -> 'env

(ffi pii-dirlist "sys/pii/lisp_dirlist" 0)
; (pii-dirlist path) -> info
(ffi pii-fstat "sys/pii/lisp_fstat" 0)
; (pii-fstat path) -> info
(ffi pii-read-char "sys/pii/lisp_readchar" 0)
; (pii-read-char fd) -> char
(ffi pii-write-char "sys/pii/lisp_writechar" 0)
; (pii-write-char fd char) -> char
(ffi pii-remove "sys/pii/lisp_remove" 0)
; (pii-remove path)
(ffi pii-time "sys/pii/lisp_time" 0)
; (pii-time) -> ns

(ffi get-field "class/obj/lisp_get_field" 0)
; (get-field obj field size|0) -> val
(ffi set-field "class/obj/lisp_set_field" 0)
; (set-field obj field size|0 val) -> val
(ffi weak-ref "class/obj/lisp_wref" 0)
; (weak-ref form) -> num
(ffi obj-ref "class/obj/lisp_oref" 0)
; (obj-ref num) -> form

(defq *root_env* (env)
	+byte_size 1 +short_size 2 +int_size 4 +long_size 8 +ptr_size 8
	+str_data 24 +mailbox_id_size 8 +node_id_size 16 +net_id_size 24
	defmacro `(,macro (n a &rest _)
		(,if (,get n (,penv)) (,throw "Macro override !, use redefmacro ? !" n))
		`(defq ,n (macro ,a ~_))))

(defmacro defun (n a &rest _)
	; (defun name ([arg ...]) body)
	(if (get n (penv)) (throw "Function override !, use redefun ?" n))
	`(defq ,n (lambda ,a ~_)))

(defmacro redefun (n a &rest _)
	; (redefun name ([arg ...]) body)
	`(defq ,n (lambda ,a ~_)))

(defmacro redefmacro (n a &rest _)
	; (redefmacro name ([arg ...]) body)
	`(defq ,n (macro ,a ~_)))

(defmacro const (_)
	; (const form)
	(eval (macroexpand _)))

(defun exec (_)
	; (exec form)
	(eval (prebind (macroexpand _))))

(defmacro static-q (_)
	; (static-q form) -> 'form
	;static quoted
	(push (list (const quote)) (prebind (macroexpand _))))

(defmacro static-qq (_)
	; (static-qq form) -> `form
	;static quasi-quoted
	(push (list (const quasi-quote)) (prebind (macroexpand _))))

;;;;;;;;;;;;;
; Debug stubs
;;;;;;;;;;;;;

(defun debug-brk (name condtion))
	; (debug-brk name condtion)

(defun profile-report (name &optional reset))
	; (profile-report name [reset])

;;;;;;;;;;;;;;
; Default opts
;;;;;;;;;;;;;;

(defmacro inc (_)
	; (inc num) -> num
	`(+ ,_ 1))

(defmacro dec (_)
	; (dec num) -> num
	`(- ,_ 1))

(defmacro opt (x y &optional z)
	; (opt sym val [cond])
	(if z `(if ,x ,z ,y) `(if ,x ,x ,y)))

(defmacro setd (&rest _)
	; (setd sym val [sym val] ...)
	(defq i -2 l (list 'setq))
	(while (< (setq i (+ i 2)) (length _))
		(push l (elem-get _ i) (list 'opt (elem-get _ i) (elem-get _ (inc i))))) l)

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun lambda? (_)
	; (lambda? form) -> :t | :nil
	(cond ((eql _ (const lambda))) ((eql _ 'lambda))))

(defun macro? (_)
	; (macro? form) -> :t | :nil
	(cond ((eql _ (const macro))) ((eql _ 'macro))))

(defun quote? (_)
	; (quote? form) -> :t | :nil
	(cond ((eql _ (const quote))) ((eql _ 'quote))))

(defun quasi-quote? (_)
	; (quasi-quote? form) -> :t | :nil
	(cond ((eql _ (const quasi-quote))) ((eql _ 'quasi-quote))))

(defun array? (_)
	; (array? form) -> :t | :nil
	(find :array (type-of _)))

(defun list? (_)
	; (list? form) -> :t | :nil
	(find-rev :list (type-of _)))

(defun num? (_)
	; (num? form) -> :t | :nil
	(find :num (type-of _)))

(defun fixed? (_)
	; (fixed? form) -> :t | :nil
	(find :fixed (type-of _)))

(defun real? (_)
	; (real? form) -> :t | :nil
	(find :real (type-of _)))

(defun nums? (_)
	; (nums? form) -> :t | :nil
	(find :nums (type-of _)))

(defun fixeds? (_)
	; (fixeds? form) -> :t | :nil
	(find :fixeds (type-of _)))

(defun reals? (_)
	; (reals? form) -> :t | :nil
	(find :reals (type-of _)))

(defun func? (_)
	; (func? form) -> :t | :nil
	(find :func (type-of _)))

(defun str? (_)
	; (str? form) -> :t | :nil
	(find :str (type-of _)))

(defun sym? (_)
	; (sym? form) -> :t | :nil
	(find-rev :sym (type-of _)))

(defun env? (_)
	; (env? form) -> :t | :nil
	(find :hmap (type-of _)))

(defun seq? (_)
	; (seq? form) -> :t | :nil
	(find :seq (type-of _)))

(defun lambda-func? (_)
	; (lambda-func? form) -> :t | :nil
	(if (list? _) (if (> (length _) 0) (lambda? (first _)))))

(defun macro-func? (_)
	; (macro-func? form) -> :t | :nil
	(if (list? _) (if (> (length _) 0) (macro? (first _)))))

(defun nil? (_)
	; (nil? o) -> :t | :nil
	(eql _ :nil))

(defun empty? (_)
	; (empty? form) -> :t | :nil
	(if (seq? _) (= (length _) 0) :t))

(defun nempty? (_)
	; (nempty? form) -> :t | :nil
	(if (seq? _) (> (length _) 0)))

(defun lisp-node? (_)
	; (lisp_node? node) -> :t | :nil
	(= (logand (code (first _)) 1) 1))

(defun cpp-node? (_)
	; (cpp_node? node) -> :t | :nil
	(= (logand (code (first _)) 1) 0))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro not (_)
	; (not form) -> :t | :nil
	`(if ,_ :nil :t))

(defmacro when (x &rest _)
	; (when tst body)
	(if (= (length _) 1)
		`(if ,x ~_)
		`(cond (,x ~_))))

(defmacro unless (x &rest _)
	; (unless tst body)
	(if (= (length _) 1)
		`(if ,x :nil ~_)
		`(cond (,x :nil) (:t ~_))))

(defmacro until (x &rest _)
	; (until tst body)
	`(while (not ,x) ~_))

(defmacro or (&rest _)
	; (or [tst] ...) -> :nil | tst
	(map! 0 -1 (const list) (list _) (list (const cond))))

(defmacro and (&rest _)
	; (and [tst] ...) -> :t | :nil | tst
	(cond
		((= (length _) 0))
		((= (length _) 1) (first _))
		(:t `(if ,(first _) (and ~(rest _))))))

(defmacro times (c &rest _)
	; (times num body)
	`(progn (defq ,(defq _c (gensym)) ,c)
		(while (<= 0 (setq ,_c (dec ,_c))) ~_)))

;;;;;;;;;;;;;;;;;;;;
; sort and randomise
;;;;;;;;;;;;;;;;;;;;

(defun sort (_a &optional _f _l _h)
	; (sort list [fcmp start end]) -> list
	(setd _f (const cmp) _l 0 _h (length _a))
	(defq _q (list _l _h))
	(while (setq _h (pop _q) _l (pop _q))
		(when (< _l _h)
			(defq _p (pivot _f _a _l _h))
			(push (push _q _l _p) (inc _p) _h))) _a)

(defun swap (_ _1 _2)
	; (swap list index index)
	(when (/= _1 _2)
		(defq _t (elem-get _ _1))
		(elem-set _ _1 (elem-get _ _2))
		(elem-set _ _2 _t)))

(defun shuffle (_a &optional _l _h)
	; (shuffle list [start end]) -> list
	(setd _l 0 _h (length _a))
	(each! _h _l (lambda (x)
		(swap _a _ (random (inc _)))) (list _a)) _a)

;;;;;;;;;;;;;;;;;
; lambda shorcuts
;;;;;;;;;;;;;;;;;

(defun walk-list (_l _f _fi _fo)
	; (walk-list list fnc_element fnc_in fnc_out)
	;if fnc_in returns :nil, it'll step down into that list.
	;fnc_out is allways called to balence calls to fnc_in.
	(defq _stack (list _l 0))
	(while (defq _ (pop _stack) _l (pop _stack))
		(cond
			((= (length _l) 0)
				;zero length list
				(_fi _l) (_fo _l))
			((= _ 0)
				;enter list ?
				(cond
					((_fi _l)
						(_fo _l))
					(:t (push _stack _l 1)
						(if (list? (defq _e (first _l))) (push _stack _e 0))
						(_f _e))))
			((< _ (length _l))
				;element of list
				(push _stack _l (inc _))
				(if (list? (defq _e (elem-get _l _))) (push _stack _e 0))
				(_f _e))
			(:t ;exit list
				(_fo _l)))))

(defmacro # (&rest _)
	; (# (< %9 %0 %3) ...) -> (lambda (%0 %3 %9) (< %9 %0 %3) ...)
	(defq args (list))
	(walk-list _
		;element func
		(lambda (_)
			(and (sym? _) (find _ '(%0 %1 %2 %3 %4 %5 %6 %7 %8 %9))
				(merge-obj args (list _))))
		;enter list func
		(lambda (_)
			(and (> (length _) 0) (find (first _)
				'(# quote quasi-quote static-q static-qq))))
		;exit list func
		progn)
	(setq args (sort args))
	`(lambda ,args ~_))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defun range (b e &optional s)
	; (range start end [step]) -> list
	(defq s (opt s 1 (abs s)) l (cap (/ (abs (- b e)) s) (list)))
	(if (<= b e)
		(while (< b e) (push l b) (setq b (+ b s)))
		(while (> b e) (push l b) (setq b (- b s)))) l)

(defun each-mergeable (_f _l)
	; (each-mergeable lambda seq) -> seq
	(defq _ -1)
	(while (< (setq _ (inc _)) (length _l))
		(_f (elem-get _l _))) _l)

(defmacro each (f &rest _)
	; (each lambda seq ...)
	`(each! 0 -1 ,f (list ~_)))

(defmacro each-rev (f &rest _)
	; (each-rev lambda seq ...)
	`(each! -1 0 ,f (list ~_)))

(defmacro map (f &rest _)
	; (map lambda seq ...) -> list
	`(map! 0 -1 ,f (list ~_)))

(defmacro map-rev (f &rest _)
	; (map-rev lambda seq ...) -> list
	`(map! -1 0 ,f (list ~_)))

(defmacro reduce (f s &optional i)
	; (reduce lambda seq [init]) -> form
	(if i
		`(reduce! 0 -1 ,f (list ,s) ,i)
		`(reduce! 1 -1 ,f (list (defq ,(defq i (gensym)) ,s)) (first ,i))))

(defmacro reduce-rev (f s &optional i)
	; (reduce-rev lambda seq [init]) -> form
	(if i
		`(reduce! -1 0 ,f (list ,s) ,i)
		`(reduce! -2 0 ,f (list (defq ,(defq i (gensym)) ,s)) (last ,i))))

(defmacro reverse (s)
	; (reverse seq) -> seq
	`(slice ,s -1 0))

;;;;;;;;
; Scopes
;;;;;;;;

(defmacro let (l &rest _)
	; (let ([(sym val) ...]) body)
	(static-qq ((lambda ,(map (const first) l) ~_) ~(map (const second) l))))

(defmacro let* (l &rest _)
	; (let* ([(sym val) ...]) body)
	(static-qq ((lambda () ,(reduce (lambda (l (var val)) (push l var val)) l (list defq)) ~_))))

;;;;;;;;;;;;;;;;;;;;;
; Sequence predicates
;;;;;;;;;;;;;;;;;;;;;

(defmacro some (f &rest _)
	; (some lambda seq ...) -> :nil | form
	`(some! 0 -1 :nil ,f (list ~_)))

(defmacro some-rev (f &rest _)
	; (some-rev lambda seq ...) -> :nil | form
	`(some! -1 0 :nil ,f (list ~_)))

(defmacro every (f &rest _)
	; (every lambda seq ...) -> :nil | form
	`(some! 0 -1 :t ,f (list ~_)))

(defmacro notany (f &rest _)
	; (notany lambda seq ...) -> :t | :nil
	`(not (some! 0 -1 :nil ,f (list ~_))))

(defmacro notevery (f &rest _)
	; (notevery lambda seq ...) -> :t | :nil
	`(not (some! 0 -1 :t ,f (list ~_))))

;;;;;;;;;;;
; Sequences
;;;;;;;;;;;

(defun lists (n)
	; (lists n) -> ((list0) ... (listn-1))
	(defq out (cap n (list)))
	(while (>= (setq n (dec n)) 0) (push out (list)))
	out)

(defun starts-with (p _)
	; (starts-with str str) -> :t | :nil
	(and (>= (length _) (length p)) (eql p (slice _ 0 (length p)))))

(defun ends-with (p _)
	; (ends-with str str) -> :t | :nil
	(and (>= (length _) (length p)) (eql p (slice _ (dec (neg (length p))) -1))))

(defun erase (s b e)
	; (erase seq start end) -> seq
	(cat (slice s 0 b) (slice s e -1)))

(defun insert (s p i)
	; (insert seq pos seq) -> seq
	(cat (slice s 0 p) i (slice s p -1)))

(defun replace (s b e i)
	; (replace seq start end seq) -> seq
	(cat (slice s 0 b) i (slice s e -1)))

(defun rotate (s i j k)
	; (rotate seq start mid end) -> seq
	(cat (slice s 0 i) (slice s j k) (slice s i j) (slice s k -1)))

(defun join (seqs seq &optional mode)
	; (join seqs seq [mode]) -> seq
	(setd mode 0)
	(defq seq (partition seq) seqs (partition seqs)
		out (if (= (logand mode 1) 0) (list) (cat seq))
		out (reduce (lambda (x y) (push x y) (each (lambda (y) (push x y)) seq) x)
			seqs (cap (+ (length seqs) (* (length seqs) (length seq)) mode) out)))
	(if (= (logand mode 2) 0) (each (lambda (x) (pop out)) seq))
	(apply (const cat) out))

(defun unzip (seq cnt)
	; (unzip seq cnt) -> seqs
	(map (# (if (= (length %0) 0) (slice seq 0 0) (apply (const cat) %0)))
		(reduce (# (push (elem-get %0 (% _ cnt)) %1) %0)
			(partition seq) (lists cnt))))

(defun zip (&rest seqs)
	; (zip seq ...) -> seq
	(if (= (length (defq out (map! 0 -1 (const cat) (map (const partition) seqs)))) 0)
		(slice (first seqs) 0 0) (apply (const cat) out)))

(defun filter-array (_f _a)
	; (filter-array lambda array) -> array
	(defq _l (slice _a 0 0))
	(each (lambda (_p) (if (_f _p) (push _l _p))) _a)
	_l)

(defun unique (seq)
	; (unique seq) -> seq
	(cond
		((= (length seq) 0)
			(rest seq))
		((array? seq)
			(reduce! 1 -1 (# (if (eql %1 (last %0)) %0 (push %0 %1)))
				(list seq) (slice seq 0 1)))
		(:t ;string
			(apply (const cat)
				(reduce (# (if (eql %1 (last %0)) %0 (push %0 %1)))
					seq (list))))))

(defun flatten (_)
	; (flatten list) -> list
	(defq out (list))
	(walk-list _
		;element func
		(lambda (_) (unless (list? _) (push out _)))
		;enter list func
		(lambda (_) :nil)
		;exit list func
		progn) out)

;;;;;;
; Case
;;;;;;

(defmacro case (_form &rest _body)
	; (case form [(key|(key ...) body)] ...)
	(defq _default_key :nil _default_clause :nil _atoms :t
		_map (reduce (lambda (_map (_keys &rest _clause_body))
			(unless (list? _keys) (setq _keys (list _keys)))
			(setq _clause_body (pop (prebind (list (macroexpand
				(if (= (length _clause_body) 1)
					(first _clause_body)
					(cat '(progn) _clause_body)))))))
			(or (eql :num (defq _clause_type (pop (type-of _clause_body))))
				(eql :str _clause_type) (setq _atoms :nil))
			(each (#
				(and (sym? %0) (starts-with "+" %0) (setq %0 (eval %0)))
				(cond
					((eql %0 :t)
						(setq _default_key :t _default_clause _clause_body))
					(:t (push (first _map) %0)
						(push (second _map) _clause_body)))) _keys) _map)
			_body (list (list) (list))))
	(cond
		(_default_key
			(push (second _map) _default_clause)
			(if _atoms
				`(elem-get ',(second _map) (or (find ,_form ',(first _map)) -2))
				`(eval (elem-get ',(second _map) (or (find ,_form ',(first _map)) -2)))))
		(:t (if _atoms
				`(if (defq ,(defq _i (gensym)) (find ,_form ',(first _map)))
					(elem-get ',(second _map) ,_i))
				`(if (defq ,(defq _i (gensym)) (find ,_form ',(first _map)))
					(eval (elem-get ',(second _map) ,_i)))))))

;;;;;;;;;;;;;;;;
; Math functions
;;;;;;;;;;;;;;;;

(defq +min_long (<< -1 63) +max_long (>> -1 1) +min_int (<< -1 31) +max_int (>> -1 33))

(defun neg? (_)
	; (neg? num) -> :t | :nil
	(< _ 0))

(defun pos? (_)
	; (pos? num) -> :t | :nil
	(> _ 0))

(defun odd? (_)
	; (odd? num) -> :t | :nil
	(= 1 (logand 1 _)))

(defun even? (_)
	; (even? num) -> :t | :nil
	(= 0 (logand 1 _)))

(defun lognot (_)
	; (lognot num) -> num
	(logxor _ -1))

(defun log2 (_)
	; (log2 num) -> num
	(when (and (not (= 0 _)) (= _ (logand _ (neg _))))
		(defq i 0)
		(while (/= 0 (setq _ (>> _ 1)))
			(setq i (inc i))) i))

(defun pow (base exponent)
	; (pow base exponent) -> integer
	(defq res 1)
	(times exponent (setq res (* res base)))
	res)

(defun ntz (_)
	; (ntz num) -> num
	(defq n 64)
	(while (/= _ 0)
		(setq n (dec n) _ (<< _ 1))) n)

(defun nto (_)
	; (nto num) -> num
	(defq n 64 _ (lognot _))
	(while (/= _ 0)
		(setq n (dec n) _ (<< _ 1))) n)

(defun nlz (_)
	; (nlz num) -> num
	(defq n 0)
	(while (> _ 0)
		(setq n (inc n) _ (<< _ 1))) n)

(defun nlo (_)
	; (nlo num) -> num
	(defq n 0 _ (lognot _))
	(while (> _ 0)
		(setq n (inc n) _ (<< _ 1))) n)

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq +fp_shift 16 +fp_int_mask (<< -1 +fp_shift) +fp_frac_mask (lognot +fp_int_mask)
	+fp_2pi 6.283185 +fp_pi 3.141592 +fp_hpi 1.570796 +fp_rpi 0.318309)

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun usort (_a &optional _f _l _h)
	; (usort list [fcmp start end]) -> list
	(unique (sort _a _f _l _h)))

(defun export (e symbols)
	; (export env symbols)
	(each (# (def e %0 (get %0))) symbols))

(defun export-symbols (symbols)
	; (export-symbols symbols)
	(export (penv (penv)) symbols))

(defun export-classes (classes)
	; (export-classes classes)
	(export (penv (penv)) (reduce
		(# (push %0 %1 (sym (cat "*class_" %1 "*")) (sym (cat %1 "?")))) classes (list))))

(defmacro ascii-code (_)
	; (ascii-code char) -> num
	(code _))

(defmacro ascii-char (_)
	; (ascii-char num) -> char
	(char (eval _)))

(defun ascii-upper (_)
	; (ascii-upper num) -> num
	(if (<= (ascii-code "a") _ (ascii-code "z"))
		(- _ (const (- (ascii-code "a") (ascii-code "A")))) _))

(defun ascii-lower (_)
	; (ascii-lower num) -> num
	(if (<= (ascii-code "A") _ (ascii-code "Z"))
		(+ _ (const (- (ascii-code "a") (ascii-code "A")))) _))

(defun to-upper (_)
	; (to-upper str) -> str
	(if (eql _ "") ""
		(apply (const cat) (map (# (char (ascii-upper (code %0)))) _))))

(defun to-lower (_)
	; (to-lower str) -> str
	(if (eql _ "") ""
		(apply (const cat) (map (# (char (ascii-lower (code %0)))) _))))

(defun align (num div)
	; (align num div) -> num
	(if (= (% num div) 0) num
		(+ (* (/ num div) div) div)))

(defun char-to-num (_)
	; (char-to-num char) -> num
	(elem-get (const (cat (list quote) (list (map num-intern (cat
		(map (# (logior %0 -1)) (range 0 (code "0")))                       ;-1
		(map (# (- %0 (code "0"))) (range (code "0") (inc (code "9"))))     ;0-9
		(map (# (logior %0 -1)) (range (inc (code "9")) (code "A")))        ;-1
		(map (# (- %0 (code "A") -10)) (range (code "A") (inc (code "Z")))) ;10-35
		(map (# (logior %0 -1)) (range (inc (code "Z")) (code "a")))        ;-1
		(map (# (- %0 (code "a") -10)) (range (code "a") (inc (code "z")))) ;10-35
		(map (# (logior %0 -1)) (range (inc (code "z")) 256)))))))          ;-1
		(code _)))

(defun num-to-char (_)
	; (num-to-char num) -> char
	(elem-get "0123456789abcdefghijklmnopqrstuvwxyz" _))

(defun str-as-num (_)
	; (str-as-num str) -> num
	(pop (nums (str-to-num _))))

(defun num-to-utf8 (_)
	; (num-to-utf8 num) -> str
	(cond
		((>= _ 0x10000)
			(char (+ 0x808080f0 (>> _ 18) (logand (>> _ 4) 0x3f00)
				(logand (<< _ 10) 0x3f0000) (logand (<< _ 24) 0x3f000000)) 4))
		((>= _ 0x800)
			(char (+ 0x8080e0 (>> _ 12) (logand (<< _ 2) 0x3f00)
				(logand (<< _ 16) 0x3f0000)) 3))
		((>= _ 0x80)
			(char (+ 0x80c0 (>> _ 6) (logand (<< _ 8) 0x3f00)) 2))
		(:t (char _))))

(defun byte-to-hex-str (_)
	; (byte-to-hex-str num) -> str
	(cat (num-to-char (logand (>> _ 4) 0xf)) (num-to-char (logand _ 0xf))))

(defun short-to-hex-str (_)
	; (short-to-hex-str num) -> str
	(cat (byte-to-hex-str (>> _ 8)) (byte-to-hex-str (logand _ 0xff))))

(defun int-to-hex-str (_)
	; (int-to-hex-str num) -> str
	(cat (short-to-hex-str (>> _ 16)) (short-to-hex-str (logand _ 0xffff))))

(defun long-to-hex-str (_)
	; (long-to-hex-str num) -> str
	(cat (int-to-hex-str (>> _ 32)) (int-to-hex-str (logand _ 0xffffffff))))

(defun trim-start (s &optional cls)
	; (trim-start str [cls]) -> str
	(if (= (setq cls (bskip (opt cls " ") s 0)) 0) s (slice s cls -1)))

(defun trim-end (s &optional cls)
	; (trim-end str [cls]) -> str
	(setd cls " ")
	(while (bfind (last s) cls) (setq s (slice s 0 -2))) s)

(defun trim (s &optional cls)
	; (trim str [cls]) -> str
	(trim-start (trim-end s cls) cls))

(defun split (s &optional cls)
	; (split str [cls]) -> strs
	(defq i 0 out (list) l (length s) cls (opt cls " "))
	(while (< i l)
		(if (/= (defq j (bskip cls s i)) i) (setq i j))
		(if (/= (defq j (bskipn cls s i)) i) (push out (slice s i (setq i j))))) out)

(defun pad (v c &optional f)
	; (pad form width [str]) -> str
	(defq f (opt f " ") v (str v) l (length v) c (- (max c l) l))
	(while (> c (length f)) (setq f (cat f f)))
	(cat (slice f 0 c) v))

(defmacro get-ubyte (s o)
	; (get-ubyte str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,(neg +byte_size)))

(defmacro get-ushort (s o)
	; (get-ushort str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,(neg +short_size)))

(defmacro get-uint (s o)
	; (get-uint str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,(neg +int_size)))

(defmacro get-long (s o)
	; (get-long str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,+long_size))

(defmacro get-byte (s o)
	; (get-byte str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,+byte_size))

(defmacro get-short (s o)
	; (get-short str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,+short_size))

(defmacro get-int (s o)
	; (get-int str index) -> num
	`(get-field ,s (+ ,+str_data ,o) ,+int_size))

(defmacro get-nodeid (s o)
	; (get-nodeid str index) -> nodeid
	`(slice ,s ,o (+ ,o ,+node_id_size)))

(defmacro get-netid (s o)
	; (get-netid str index) -> netid
	`(slice ,s ,o (+ ,o ,+net_id_size)))

(defun get-cstr (s o)
	; (get-cstr str index) -> str
	(defq k o)
	(while (/= 0 (get-byte s o))
		(setq o (inc o)))
	(slice s k o))

(defun type-to-size (_)
	; (type-to-size sym) -> num
	(case _
		((i ui) +int_size)
		((s us) +short_size)
		((b ub) +byte_size)
		(:t +long_size)))

(defun time-in-seconds (_)
	; (time-in-seconds time) -> str
	(str (/ _ 1000000) "." (pad (% _ 1000000) 6 "00000")))

(defun lisp-nodes ()
	; (lisp-nodes) -> nodes
	(filter-array (const lisp-node?) (mail-nodes)))

(defun max-length (_)
	; (max-length list) -> max
	(reduce (# (max %0 (length %1))) _ 0))

(defun min-length (_)
	; (min-length list) -> min
	(if (= (length _) 0) 0
		(reduce (# (min %0 (length %1))) _ +max_long)))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun age (_)
	; (age path) -> 0 | time ns
	(if (setq _ (pii-fstat _)) (first _) 0))

(defun load-stream (_)
	; (load-stream path) -> :nil | stream
	(if (defq _ (load _)) (string-stream _)))

(defun each-line (_f _s &optional _)
	; (each-line lambda stream [line_num])
	(setd _ 0)
	(while (defq _l (read-line _s))
		(_f _l) (setq _ (inc _))))

(defun abs-path (lib_path &optional current)
	; (abs-path path [current]) -> path
	(cond
		((not (starts-with "./" lib_path))
			lib_path)
		(:t (setd current *stream_name*)
			(while (starts-with "./" lib_path)
				(unless (defq i (find-rev "/" current))
					(throw "No relative path !" lib_path))
				(setq current (slice current 0 i)
					lib_path (slice lib_path 2 -1)))
			(cat current "/" lib_path))))

(defun import (lib_path &optional _e)
	; (import path [env]) -> env
	(unless (eql :str (pop (type-of lib_path))) (throw "Not a string !" lib_path))
	(setq lib_path (abs-path lib_path))
	(if (= (age lib_path) 0) (throw "No such file !" lib_path))
	(setd _e (penv)) (defq _ee _e)
	; does the import allready exist in the environment tree ?
	(while (env? _ee)
		(cond
			((and (defq file_list (def? '*file_includes* _ee))
					(find lib_path file_list))
				(setq _ee :t))
			(:t (setq _ee (penv _ee)))))
	; if not existing import it here
	(unless _ee
		(unless (def? '*file_includes* _e)
			(def _e '*file_includes* (list)))
		(push (get '*file_includes* _e) lib_path)
		(eval `(repl (file-stream ,lib_path) ,lib_path) _e)))

(defun import-from (lib_path &optional symbols classes)
	; (import-from [symbols classes])
	(unless (eql :str (pop (type-of lib_path))) (throw "Not a string !" lib_path))
	(setq lib_path (abs-path lib_path))
	(if (= (age lib_path) 0) (throw "No such file !" lib_path))
	(setd symbols '() classes '())
	;import it here, we will discard it on exit
	(repl (file-stream lib_path) lib_path)
	;export only what was requested
	(export-symbols symbols)
	(export-classes classes))

(defmacro read-long (s)
	; (read-long stream) -> num
	`(read-char ,s 8))

(defmacro read-int (s)
	; (read-int stream) -> num
	`(read-char ,s 4))

(defmacro read-short (s)
	; (read-short stream) -> num
	`(read-char ,s 2))

(defmacro write-long (s n)
	; (write-long stream num|list) -> stream
	`(write-char ,s ,n 8))

(defmacro write-int (s n)
	; (write-int stream num|list) -> stream
	`(write-char ,s ,n 4))

(defmacro write-short (s n)
	; (write-short stream num|list) -> stream
	`(write-char ,s ,n 2))

(defun write-line (s _)
	; (write-line stream str) -> stream
	(write-char (write s _) 10))

(defun read-data (s bytes)
	; (read-data stream bytes) -> str
	(defq data (cap bytes (list)))
	(while (and (>= (setq bytes (dec bytes)) 0) (defq _ (read-char s)))
		(push data (char _)))
	(apply (const cat) data))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Compilation environment
;;;;;;;;;;;;;;;;;;;;;;;;;

;compilation options
;debug_mode 0, release, strip all error checking
;debug_mode 1, normal, with error checking
(defq *debug_mode* 1 *debug_emit* :nil *debug_inst* :nil)

(defun os ()
	; (os) -> sym
	(defq o 'Linux)
	(when (defq f (file-stream 'os))
		(bind '(o _) (read f))) o)

(defun cpu ()
	; (cpu) -> sym
	(defq o 'x86_64)
	(when (defq f (file-stream 'cpu))
		(bind '(o _) (read f))) o)

(defun abi ()
	; (abi) -> sym
	(defq o 'AMD64)
	(when (defq f (file-stream 'abi))
		(bind '(o _) (read f))) o)

(defun within-compile-env (_f)
	; (within-compile-env lambda)
	(defq *compile_env* (env-resize 499))
	(defmacro defcvar (&rest b) `(def *compile_env* ~b))
	(defmacro deffvar (&rest b) `(def *func_env* ~b))
	(defun include (lib_path) (import lib_path *compile_env*))
	(catch (progn (setq _f (_f)) (undef (env) '*func_env* '*compile_env*) _f)
		(progn (undef (penv) '*func_env* '*compile_env*) :nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Boot extension imports
;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/class/struct.inc")
(import "lib/class/class.inc")
(import "lib/collections/collections.inc")
(import "lib/text/searching.inc")
