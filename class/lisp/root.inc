;;;;;;;;;;;;
; Primitives
;;;;;;;;;;;;

;these are build in by the lisp class init function !!!
; (ffi "class/lisp/lisp_ffi" ffi 1)
; (ffi "class/lisp/lisp_lambda" lambda 1)
; (ffi "class/lisp/lisp_macro" macro 1)
; (ffi "class/lisp/lisp_quote" quote 1)
; (ffi "class/lisp/lisp_qquote" quasi-quote 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Language and Control Flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/lisp/lisp_progn" progn 1)
; (progn [body]) -> 'form
(ffi "class/lisp/lisp_catch" catch 1)
; (catch form eform) -> 'form
(ffi "class/lisp/lisp_cond" cond 1)
; (cond [(tst body)] ...) -> 'form
(ffi "class/lisp/lisp_condn" condn 1)
; (condn [(tst body)] ...) -> 'form
(ffi "class/lisp/lisp_if" if 1)
; (if tst form [else_form]) -> 'form
(ffi "class/lisp/lisp_ifn" ifn 1)
; (ifn tst form [else_form]) -> 'form
(ffi "class/lisp/lisp_while" while 1)
; (while tst [body]) -> :nil
(ffi "class/lisp/lisp_until" until 1)
; (until tst [body]) -> tst
(ffi "class/lisp/lisp_throw" throw)
; (throw str form)
(ffi "class/lisp/lisp_bind" bind)
; (bind (sym ...) seq) -> val
(ffi "class/lisp/lisp_read" read)
; (read stream [last_char]) -> :nil | (form next_char)
(ffi "class/lisp/lisp_eval" eval)
; (eval form [env]) -> 'form
(ffi "class/lisp/lisp_eval_list" eval-list)
; (eval-list list [env]) -> list
(ffi "class/lisp/lisp_identity" identity)
; (identity [form]) -> :nil | form
(ffi "class/lisp/lisp_apply" apply)
; (apply lambda seq) -> form
(ffi "class/lisp/lisp_repl" repl)
; (repl stream name) -> form
(ffi "class/lisp/lisp_repl_info" repl-info 1)
; (repl-info) -> (name line)
(ffi "class/lisp/lisp_prin" prin)
; (prin [form] ...) -> form
(ffi "class/lisp/lisp_print" print)
; (print [form] ...) -> form
(ffi "class/lisp/lisp_prebind" prebind)
; (prebind form) -> form
(ffi "class/lisp/lisp_mcall" .)
; (. env sym [...]) -> form
(ffi "class/lisp/lisp_macroexpand" macroexpand)
; (macroexpand form) -> 'form
(ffi "class/lisp/lisp_eql" eql)
; (eql form form) -> :nil | :t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Numeric and Vector Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/num/lisp_eq" =)
; (= num num ...) -> :t | :nil
(ffi "class/num/lisp_ne" /=)
; (/= num num ...) -> :t | :nil
(ffi "class/num/lisp_lt" <)
; (< num num ...) -> :t | :nil
(ffi "class/num/lisp_gt" >)
; (> num num ...) -> :t | :nil
(ffi "class/num/lisp_le" <=)
; (<= num num ...) -> :t | :nil
(ffi "class/num/lisp_ge" >=)
; (>= num num ...) -> :t | :nil
(ffi "class/num/lisp_add" +)
; (+ num num ...) -> num
(ffi "class/num/lisp_sub" -)
; (- num num ...) -> num
(ffi "class/num/lisp_mul" *)
; (* num num ...) -> num
(ffi "class/num/lisp_div" /)
; (/ num num ...) -> num
(ffi "class/num/lisp_mod" %)
; (% num num ...) -> num
(ffi "class/num/lisp_min" min)
; (min num num ...) -> num
(ffi "class/num/lisp_max" max)
; (max num num ...) -> num
(ffi "class/num/lisp_neg" neg)
; (neg num) -> num
(ffi "class/num/lisp_abs" abs)
; (abs num) -> num
(ffi "class/num/lisp_sqrt" sqrt)
; (sqrt num) -> num
(ffi "class/num/lisp_sign" sign)
; (sign num) -> -1 | 0 | 1
(ffi "class/num/lisp_n2i" n2i)
; (n2i num) -> num
(ffi "class/num/lisp_n2f" n2f)
; (n2f num) -> fixed
(ffi "class/num/lisp_n2r" n2r)
; (n2r num) -> real
(ffi "class/num/lisp_random" random)
; (random num) -> num
(ffi "class/num/lisp_intern" num-intern)
; (num-intern num) -> num
(ffi "class/num/lisp_shr" >>)
; (>> num cnt) -> num
(ffi "class/num/lisp_asr" >>>)
; (>>> num cnt) -> num
(ffi "class/num/lisp_shl" <<)
; (<< num cnt) -> num
(ffi "class/num/lisp_and" logand)
; (logand [num] ...) -> num
(ffi "class/num/lisp_or" logior)
; (logior [num] ...) -> num
(ffi "class/num/lisp_xor" logxor)
; (logxor [num] ...) -> num
(ffi "class/fixed/lisp_sin" sin)
; (sin fixed) -> fixed
(ffi "class/fixed/lisp_cos" cos)
; (cos fixed) -> fixed
(ffi "class/fixed/lisp_frac" frac)
; (frac fixed) -> fixed
(ffi "class/fixed/lisp_floor" floor)
; (floor fixed) -> fixed
(ffi "class/fixed/lisp_recip" recip)
; (recip fixed) -> fixed
(ffi "class/nums/lisp_abs" nums-abs)
; (nums-abs nums [nums]) -> nums
(ffi "class/nums/lisp_scale" nums-scale)
; (nums-scale nums scale [nums]) -> nums
(ffi "class/nums/lisp_add" nums-add)
; (nums-add nums nums [nums]) -> nums
(ffi "class/nums/lisp_div" nums-div)
; (nums-div nums nums [nums]) -> nums
(ffi "class/nums/lisp_mod" nums-mod)
; (nums-mod nums nums [nums]) -> nums
(ffi "class/nums/lisp_mul" nums-mul)
; (nums-mul nums nums [nums]) -> nums
(ffi "class/nums/lisp_sub" nums-sub)
; (nums-sub nums nums [nums]) -> nums
(ffi "class/nums/lisp_sum" nums-sum)
; (nums-sum nums) -> num
(ffi "class/nums/lisp_dot" nums-dot)
; (nums-dot nums nums) -> num
(ffi "class/nums/lisp_min" nums-min)
;(nums-min nums nums [nums]) -> nums
(ffi "class/nums/lisp_max" nums-max)
;(nums-max nums nums [nums]) -> nums
(ffi "class/fixeds/lisp_frac" fixeds-frac)
; (fixeds-frac fixeds [fixeds]) -> fixeds
(ffi "class/fixeds/lisp_floor" fixeds-floor)
; (fixeds-floor fixeds [fixeds]) -> fixeds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sequence, Array, and List Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/seq/lisp_length" length)
; (length seq) -> num
(ffi "class/seq/lisp_eget" elem-get)
; (elem-get seq idx) -> elem
(ffi "class/seq/lisp_first" first)
; (first seq) -> :nil | elem
(ffi "class/seq/lisp_second" second)
; (second seq) -> :nil | elem
(ffi "class/seq/lisp_third" third)
; (third seq) -> :nil | elem
(ffi "class/seq/lisp_last" last)
; (last seq) -> :nil | elem
(ffi "class/seq/lisp_rest" rest)
; (rest seq) -> empty | seq
(ffi "class/seq/lisp_most" most)
; (most seq) -> empty | seq
(ffi "class/seq/lisp_find" find)
; (find elem seq [idx]) -> :nil | idx
(ffi "class/seq/lisp_rfind" rfind)
; (rfind elem seq [idx]) -> :nil | idx
(ffi "class/seq/lisp_slice" slice)
; (slice seq start end) -> seq
(ffi "class/seq/lisp_partition" partition)
; (partition seq [cnt]) -> (seq ...)
(ffi "class/seq/lisp_cat" cat)
; (cat seq ...) -> seq
(ffi "class/seq/lisp_pling" ! 1)
; (!) -> idx
(ffi "class/seq/lisp_each" each!)
; (each! lambda seqs [start end])
(ffi "class/seq/lisp_some" some!)
; (some! lambda seqs [mode start end]) -> :nil | val
(ffi "class/seq/lisp_map" map!)
; (map! lambda seqs [out start end]) -> out | (...)
(ffi "class/seq/lisp_filter" filter!)
; (filter! lambda seq [out start end]) -> out | (...)
(ffi "class/seq/lisp_reduce" reduce!)
; (reduce! lambda seqs init [start end]) -> val
(ffi "class/array/lisp_cap" cap)
; (cap len array ...) -> array
(ffi "class/array/lisp_clear" clear)
; (clear array ...) -> array
(ffi "class/array/lisp_push" push)
; (push array elem ...) -> array
(ffi "class/array/lisp_pop" pop)
; (pop array) -> elem | :nil
(ffi "class/array/lisp_eset" elem-set)
; (elem-set array idx elem) -> array
(ffi "class/list/lisp_merge" merge)
; (merge dlist slist) -> dlist
(ffi "class/list/lisp_pivot" pivot)
; (pivot lambda list start end)
(ffi "class/list/lisp_match" lmatch?)
; (lmatch? list list) -> :nil | :t
(ffi "class/list/lisp_copy" copy)
; (copy form) -> 'form

;;;;;;;;;;;;;;;;;;;;
; String Operations
;;;;;;;;;;;;;;;;;;;;
(ffi "class/str/lisp_str" str)
; (str form) -> str
(ffi "class/str/lisp_tonum" str-to-num)
; (str-to-num str) -> num
(ffi "class/str/lisp_expand" expand)
; (expand str tab_width) -> str
(ffi "class/str/lisp_char" char)
; (char num [width]) -> str
(ffi "class/str/lisp_code" code)
; (code str [width idx]) -> num
(ffi "class/str/lisp_cmp" cmp)
; (cmp str str) -> + | 0 | -
(ffi "class/str/lisp_save" save)
; (save str path) -> str
(ffi "class/str/lisp_load" load)
; (load path) -> str
(ffi "class/str/lisp_create" str-alloc)
; (str-alloc size) -> str
(ffi "class/str/lisp_bfind" bfind)
; (bfind char cls) -> :nil | idx
(ffi "class/str/lisp_bskip" bskip)
; (bskip cls str idx) -> idx
(ffi "class/str/lisp_bskipn" bskipn)
; (bskipn cls str idx) -> idx
(ffi "class/str/lisp_rbskip" rbskip)
; (rbskip cls str idx) -> idx
(ffi "class/str/lisp_rbskipn" rbskipn)
; (rbskipn cls str idx) -> idx
(ffi "class/str/lisp_encode" hex-encode)
; (hex-encode str) -> str
(ffi "class/str/lisp_decode" hex-decode)
; (hex-decode str) -> str

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Symbol and Environment Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/sym/lisp_sym" sym)
; (sym str) -> sym
(ffi "class/sym/lisp_gensym" gensym)
; (gensym) -> sym
(ffi "class/hmap/lisp_defq" defq 1)
; (defq sym val [sym val] ...) -> val
(ffi "class/hmap/lisp_setq" setq 1)
; (setq sym val [sym val] ...) -> val
(ffi "class/hmap/lisp_def" def)
; (def env sym val [sym val] ...) -> val
(ffi "class/hmap/lisp_set" set)
; (set env sym val [sym val] ...) -> val
(ffi "class/hmap/lisp_get" get)
; (get sym [env]) -> :nil | val
(ffi "class/hmap/lisp_defx" def?)
; (def? sym [env]) -> :nil | val
(ffi "class/hmap/lisp_undef" undef)
; (undef env sym [sym] ...) -> env
(ffi "class/hmap/lisp_env" env)
; (env [num]) -> env
(ffi "class/hmap/lisp_penv" penv)
; (penv [env]) -> :nil | env
(ffi "class/hmap/lisp_resize" env-resize)
; (env-resize num [env]) -> env
(ffi "class/hmap/lisp_tolist" tolist)
; (tolist env) -> ((sym val) ...)
(ffi "class/lisp/lisp_env_push" env-push)
; (env-push [env]) -> 'env
(ffi "class/lisp/lisp_env_pop" env-pop)
; (env-pop [env]) -> 'env

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stream and I/O Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/stream/lisp_iostream" io-stream)
; (io-stream io) -> :nil | stream
(ffi "class/stream/lisp_sstream" string-stream)
; (string-stream str) -> stream
(ffi "class/stream/lisp_fstream" file-stream)
; (file-stream path [mode]) -> :nil | stream
(ffi "class/stream/lisp_readchar" read-char)
; (read-char stream [width]) -> :nil | num
(ffi "class/stream/lisp_writechar" write-char)
; (write-char stream list|num [width]) -> bytes
(ffi "class/stream/lisp_readblk" read-blk)
; (read-blk stream bytes) -> :nil | str
(ffi "class/stream/lisp_writeblk" write-blk)
; (write-blk stream str) -> bytes
(ffi "class/stream/lisp_readline" read-line)
; (read-line stream) -> :nil | str
(ffi "class/stream/lisp_writeline" write-line)
; (write-line stream str) -> bytes
(ffi "class/stream/lisp_readavail" read-avail)
; (read-avail stream) -> :nil | num
(ffi "class/stream/lisp_avail" stream-avail)
; (stream-avail stream) -> num
(ffi "class/stream/lisp_flush" stream-flush)
; (stream-flush stream) -> stream
(ffi "class/stream/lisp_seek" stream-seek)
; (stream-seek stream offset whence) -> stream
(ffi "class/stream/lisp_read_bits" read-bits)
; (read-bits stream (array bit_pool bit_pool_size) num_bits) -> (data|-1)
(ffi "class/stream/lisp_write_bits" write-bits)
; (write-bits stream (array bit_pool bit_pool_size) data num_bits) -> stream
(ffi "class/stream/lisp_mstream" memory-stream)
; (memory-stream) -> stream
(ffi "class/out/lisp_create" out-stream)
; (out-stream mbox) -> out_stream
(ffi "class/in/lisp_create" in-stream)
; (in-stream) -> in_stream
(ffi "class/in/lisp_next_msg" in-next-msg)
; (in-next-msg in_stream) -> msg
(ffi "class/stdio/lisp_create" create-stdio)
; (create-stdio) -> stdio
(ffi "class/stream/lisp_each" lines!)
; (lines! lambda stream) -> :nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Data Structure Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/array/lisp_array" array)
; (array [num ...]) -> array
(ffi "class/list/lisp_list" list)
; (list [elem ...]) -> list
(ffi "class/array/lisp_nums" nums)
; (nums [num ...]) -> nums
(ffi "class/array/lisp_fixeds" fixeds)
; (fixeds [fixed ...]) -> fixeds
(ffi "class/array/lisp_reals" reals)
; (reals [real ...]) -> reals
(ffi "class/array/lisp_path" path)
; (path [fixed ...]) -> path
(ffi "class/dim/lisp_dim" dim)
; (dim nums array) -> dim
(ffi "class/dim/lisp_dget" dim-get)
; (dim-get dim nums) -> elem
(ffi "class/dim/lisp_dset" dim-set)
; (dim-set dim nums elem) -> array

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; System, Task, and PII Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "sys/pii/lisp_dirlist" pii-dirlist)
; (pii-dirlist path) -> info
(ffi "sys/pii/lisp_fstat" pii-fstat)
; (pii-fstat path) -> info
(ffi "sys/pii/lisp_readchar" pii-read-char)
; (pii-read-char fd) -> char
(ffi "sys/pii/lisp_writechar" pii-write-char)
; (pii-write-char fd char) -> char
(ffi "sys/pii/lisp_remove" pii-remove)
; (pii-remove path)
(ffi "sys/pii/lisp_time" pii-time)
; (pii-time) -> ns

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic Object Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/obj/lisp_hash" hash)
; (hash obj) -> num
(ffi "class/obj/lisp_type" type-of)
; (type-of obj) -> (... :obj)
(ffi "class/obj/lisp_get_field" get-field)
; (get-field obj field size|0) -> val
(ffi "class/obj/lisp_set_field" set-field)
; (set-field obj field size|0 val) -> obj
(ffi "class/obj/lisp_wref" weak-ref)
; (weak-ref obj) -> num
(ffi "class/obj/lisp_oref" obj-ref)
; (obj-ref num) -> obj

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

(defmacro macrobind (_)
	; (macrobind form) -> (prebind (macroexpand form))
	`(prebind (macroexpand ,_)))

(defun exec (_)
	; (exec form)
	(eval (macrobind _)))

(defmacro const (_)
	; (const form)
	(exec _))

(defmacro static-q (_)
	; (static-q form) -> 'form
	;static quoted
	(list (const quote) (macrobind _)))

(defmacro static-qq (_)
	; (static-qq form) -> `form
	;static quasi-quoted
	(list (const quasi-quote) (macrobind _)))

(defmacro static-qqp (_)
	; (static-qqp form) -> `form
	;static quasi-quoted, prebind only !
	(list (const quasi-quote) (prebind _)))

(defmacro callback (f e &rest args)
	; (callback lambda env arg ...) -> (#eval `(#apply ,lambda '(,arg ...)) env)
	(list eval (list quasi-quote (list apply (list 'unquote f) (cat (list quote)
		(list (map! (lambda (a) (list 'unquote a)) (list args)))))) e))

;;;;;;;;;;;;;
; Debug stubs
;;;;;;;;;;;;;

(defmacro debug-brk (brk_id &optional condition) :nil)
	; (debug-brk brk_id [condition])

(defun profile-report (name &optional reset))
	; (profile-report name [reset])

;;;;;;;;;;;;;;
; Default opts
;;;;;;;;;;;;;;

(defmacro setd (&rest _)
	; (setd sym val [sym val] ...)
	(reduce! (lambda (l (s v)) (push l s (list (const ifn) s v)))
		(list (partition _ 2)) (list (const setq))))

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
	(rfind :list (type-of _)))

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
	(rfind :sym (type-of _)))

(defun env? (_)
	; (env? form) -> :t | :nil
	(find :hmap (type-of _)))

(defun seq? (_)
	; (seq? form) -> :t | :nil
	(find :seq (type-of _)))

(defun lambda-func? (_)
	; (lambda-func? form) -> :t | :nil
	(if (list? _) (lambda? (first _))))

(defun macro-func? (_)
	; (macro-func? form) -> :t | :nil
	(if (list? _) (macro? (first _))))

(defun nil? (_)
	; (nil? o) -> :t | :nil
	(eql _ :nil))

(defun atom? (_)
	; (atom? o) -> :t | :nil
	(cond ((sym? _) (eql (first _) ":")) ((list? _) :nil) (:t)))

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

(defmacro inc (_)
	; (inc num) -> num
	(static-qq (+ ,_ 1)))

(defmacro dec (_)
	; (dec num) -> num
	(static-qq (- ,_ 1)))

(defmacro ++ (_ &optional i)
	; (++ num [num]) -> num
	(static-qq (setq ,_ (+ ,_ ,(ifn i 1)))))

(defmacro -- (_ &optional i)
	; (-- num [num]) -> num
	(static-qq (setq ,_ (- ,_ ,(ifn i 1)))))

(defmacro not (_)
	; (not form) -> :t | :nil
	(static-qq (if ,_ :nil :t)))

(defmacro when (x &rest _)
	; (when tst body)
	(if (= (length _) 1)
		(static-qq (if ,x ~_))
		(static-qq (cond (,x ~_)))))

(defmacro unless (x &rest _)
	; (unless tst body)
	(if (= (length _) 1)
		(static-qq (ifn ,x ~_))
		(static-qq (condn (,x ~_)))))

(defmacro or (&rest _)
	; (or [tst] ...) -> :nil | tst
	(map! (const list) (list _) (list (const cond))))

(defmacro and (&rest _)
	; (and [tst] ...) -> :t | :nil | tst
	(map! (const list) (list _) (list (const condn))))

(defmacro times (c &rest _)
	; (times num body)
	(static-qq (progn (defq ,(defq _c (gensym)) ,c)
		(while (<= 0 (setq ,_c (dec ,_c))) ~_))))

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
	; (swap list idx idx)
	(when (/= _1 _2)
		(defq _t (elem-get _ _1))
		(elem-set (elem-set _ _1 (elem-get _ _2)) _2 _t)))

(defun shuffle (_a &optional _l _h)
	; (shuffle list [start end]) -> list
	(setd _l 0 _h (length _a))
	(each! (lambda (x) (swap _a (!) (random (inc (!)))))
		(list _a) _h _l) _a)

;;;;;;;;;;;;;;;;;;
; lambda shortcuts
;;;;;;;;;;;;;;;;;;

(defmacro # (&rest body)
	; (# (< %9 %0 %3) ...) -> (lambda (%%0 %%3 %%9) (< %%9 %%0 %%3) ...)
	;the original template args are renamed for safety !
	(defq args (list) stack (list body))
	(while (defq lst (pop stack))
		(each! (lambda (%0)
			(if (list? %0)
				(unless (find (first %0) '(# quote quasi-quote static-q static-qq static-qqp))
					(push stack %0))
				(when (and (sym? %0) (setq %0 (find %0 '(%0 %1 %2 %3 %4 %5 %6 %7 %8 %9))))
					(setq %0 (elem-get '(%%0 %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9) %0))
					(elem-set lst (!) %0)
					(merge args (list %0)))))
			(list lst)))
	(setq args (sort args))
	`(lambda ,args ~body))

;;;;;;;;;;;;
; Functional
;;;;;;;;;;;;

(defun range (b e &optional s)
	; (range start end [step]) -> list
	(defq s (ifn s 1 (abs s)) l (cap (/ (abs (- b e)) s) (list)))
	(if (<= b e)
		(while (< b e) (push l b) (++ b s))
		(while (> b e) (push l b) (-- b s))) l)

(defun each-mergeable (f s)
	; (each-mergeable lambda seq) -> seq
	(defq i -1)
	(while (< (++ i) (length s))
		(callback f (penv) (elem-get s i))) s)

(defmacro each (f &rest _)
	; (each lambda seq ...)
	(static-qq (each! ,f (list ~_))))

(defmacro reach (f &rest _)
	; (reach lambda seq ...)
	(static-qq (each! ,f (list ~_) -1 0)))

(defmacro map (f &rest _)
	; (map lambda seq ...) -> list
	(static-qq (map! ,f (list ~_))))

(defmacro rmap (f &rest _)
	; (rmap lambda seq ...) -> list
	(static-qq (map! ,f (list ~_) (list) -1 0)))

(defmacro reduce (f s &optional i)
	; (reduce lambda seq [init]) -> form
	(cond
		(i (static-qq (reduce! ,f (list ,s) ,i)))
		((list? s) (static-qq (reduce! ,f (list (defq ,(defq i (gensym)) ,s)) (first ,i) 1)))
		((static-qq (reduce! ,f (list ,s) (first ,s) 1)))))

(defmacro rreduce (f s &optional i)
	; (rreduce lambda seq [init]) -> form
	(cond
		(i (static-qq (reduce! ,f (list ,s) ,i -1 0)))
		((list? s) (static-qq (reduce! ,f (list (defq ,(defq i (gensym)) ,s)) (last ,i) -2 0)))
		((static-qq (reduce! ,f (list ,s) (last ,s) -2 0)))))

(defmacro filter (f s)
	; (filter lambda seq) -> list
	(static-qq (filter! ,f ,s)))

(defmacro reverse (s)
	; (reverse seq) -> seq
	(static-qq (slice ,s -1 0)))

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
	(static-qq (some! ,f (list ~_))))

(defmacro rsome (f &rest _)
	; (rsome lambda seq ...) -> :nil | form
	(static-qq (some! ,f (list ~_) :nil -1 0)))

(defmacro every (f &rest _)
	; (every lambda seq ...) -> :nil | form
	(static-qq (some! ,f (list ~_) :t)))

(defmacro notany (f &rest _)
	; (notany lambda seq ...) -> :t | :nil
	(static-qq (not (some! ,f (list ~_)))))

(defmacro notevery (f &rest _)
	; (notevery lambda seq ...) -> :t | :nil
	(static-qq (not (some! ,f (list ~_) :t))))

;;;;;;;;;;;
; Sequences
;;;;;;;;;;;

(defun lists (n)
	; (lists n) -> ((list0) ... (listn-1))
	(defq out (cap n (list)))
	(while (>= (-- n) 0) (push out (list)))
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
	(defq out (reduce (# (push %0 %1 seq)) seqs
		(cap (inc (* 2 (length seqs))) (if (= (logand mode 1) 0) (list) (list seq)))))
	(if (= (logand mode 2) 0) (pop out))
	(apply (const cat) out))

(defun unzip (seq cnt)
	; (unzip seq cnt) -> seqs
	(map (# (if (= (length %0) 0) (slice seq 0 0) (apply (const cat) %0)))
		(reduce (# (push (elem-get %0 (% (!) cnt)) %1) %0)
			(partition seq) (lists cnt))))

(defun zip (&rest seqs)
	; (zip seq ...) -> seq
	(if (= (length (defq out (map! (const cat) (map (const partition) seqs)))) 0)
		(slice (first seqs) 0 0) (apply (const cat) out)))

(defun unique (seq)
	; (unique seq) -> seq
	(cond
		((= (length seq) 0)
			(rest seq))
		((array? seq)
			(reduce! (# (if (eql %1 (last %0)) %0 (push %0 %1)))
				(list seq) (slice seq 0 1) 1))
		((apply (const cat)
				(reduce (# (if (eql %1 (last %0)) %0 (push %0 %1)))
					seq (list))))))

(defun flatten (lst)
	; (flatten list) -> list
	(defq out (list) stack (list lst 0))
	(while (defq idx (pop stack) lst (pop stack))
		(some! (# (cond
			((list? %0) (push stack lst (inc (!)) %0 0) :nil)
			((push out %0)))) (list lst) :t idx)) out)

;;;;;;
; Case
;;;;;;

(defmacro case (key &rest body)
	; (case form [(key|(key ...) body)] ...)
	(defq val :nil)
	(bind '(keys vals)
		(reduce (lambda (assoc (keys &rest body))
			(unless (list? keys) (setq keys (list keys)))
			(setq body (pop (macrobind
				(if (= (length body) 1) body
					(list (cat '(progn) body))))))
			(each (# (and (sym? %0) (eql (first %0) "+") (setq %0 (eval %0)))
				(cond
					((eql %0 :t) (setq val body))
					((push (first assoc) %0) (push (second assoc) body))))
				keys) assoc)
			body (list (list) (list))))
	(if (every (const atom?) (push vals val))
		(static-qq (elem-get ',vals (ifn (find ,key ',keys) -2)))
		(static-qq (eval (elem-get ',vals (ifn (find ,key ',keys) -2))))))

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
			(++ i)) i))

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
	(hex-encode (char _)))

(defun short-to-hex-str (_)
	; (short-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char _ +short_size))))))

(defun int-to-hex-str (_)
	; (int-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char _ +int_size))))))

(defun long-to-hex-str (_)
	; (long-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char _ +long_size))))))

(defun trim-start (s &optional cls)
	; (trim-start str [cls]) -> str
	(slice s (bskip (ifn cls " ") s 0) -1))

(defun trim-end (s &optional cls)
	; (trim-end str [cls]) -> str
	(slice s 0 (rbskip (ifn cls " ") s -1)))

(defun trim (s &optional cls)
	; (trim str [cls]) -> str
	(slice s (bskip (setd cls " ") s 0) (rbskip cls s -1)))

(defun split (s &optional cls)
	; (split str [cls]) -> strs
	(defq i 0 out (list) l (length s) cls (ifn cls " "))
	(while (< i l)
		(if (/= (defq j (bskip cls s i)) i) (setq i j))
		(if (/= (defq j (bskipn cls s i)) i) (push out (slice s i (setq i j))))) out)

(defun pad (v c &optional f)
	; (pad form width [str]) -> str
	(defq f (ifn f " ") v (str v) l (length v) c (- (max c l) l))
	(while (> c (length f)) (setq f (cat f f)))
	(cat (slice f 0 c) v))

(defmacro get-ubyte (s o)
	; (get-ubyte str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) (const (neg +byte_size)))))

(defmacro get-ushort (s o)
	; (get-ushort str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) (const (neg +short_size)))))

(defmacro get-uint (s o)
	; (get-uint str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) (const (neg +int_size)))))

(defmacro get-long (s o)
	; (get-long str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) +long_size)))

(defmacro get-byte (s o)
	; (get-byte str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) +byte_size)))

(defmacro get-short (s o)
	; (get-short str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) +short_size)))

(defmacro get-int (s o)
	; (get-int str idx) -> num
	(static-qq (get-field ,s (+ +str_data ,o) +int_size)))

(defmacro get-nodeid (s o)
	; (get-nodeid str idx) -> nodeid
	(static-qq (slice ,s ,o (+ ,o +node_id_size))))

(defmacro get-netid (s o)
	; (get-netid str idx) -> netid
	(static-qq (slice ,s ,o (+ ,o +net_id_size))))

(defun get-cstr (s o)
	; (get-cstr str idx) -> str
	(defq k o)
	(while (/= 0 (get-byte s o)) (++ o))
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
	(filter (const lisp-node?) (mail-nodes)))

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

(defun abs-path (lib_path &optional current)
	; (abs-path path [current]) -> path
	(cond
		((not (starts-with "./" lib_path))
			lib_path)
		(:t (setd current (first (repl-info)))
			(while (starts-with "./" lib_path)
				(unless (defq i (rfind "/" current))
					(throw "No relative path !" lib_path))
				(setq current (slice current 0 (dec i))
					lib_path (slice lib_path 2 -1)))
			(cat current "/" lib_path))))

(defun import (lib_path &optional _e)
	; (import path [env]) -> env
	(unless (eql :str (pop (type-of lib_path))) (throw "Not a string !" lib_path))
	(setq lib_path (abs-path lib_path))
	(if (= (age lib_path) 0) (throw "No such file !" lib_path))
	(setd _e (penv)) (defq _ee _e)
	; does the import already exist in the environment tree ?
	(while (env? _ee)
		(cond
			((and (defq file_list (def? '*file_includes* _ee))
					(find lib_path file_list))
				(setq _ee :t))
			((setq _ee (penv _ee)))))
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
	(static-qq (read-char ,s +long_size)))

(defmacro read-int (s)
	; (read-int stream) -> num
	(static-qq (read-char ,s +int_size)))

(defmacro read-short (s)
	; (read-short stream) -> num
	(static-qq (read-char ,s +short_size)))

(defmacro write-long (s n)
	; (write-long stream list|num) -> bytes
	(static-qq (write-char ,s ,n +long_size)))

(defmacro write-int (s n)
	; (write-int stream list|num) -> bytes
	(static-qq (write-char ,s ,n +int_size)))

(defmacro write-short (s n)
	; (write-short stream list|num) -> bytes
	(static-qq (write-char ,s ,n +short_size)))

(defun read-data (s bytes)
	; (read-data stream bytes) -> str
	(defq data (cap bytes (list)))
	(while (and (>= (-- bytes) 0) (defq _ (read-char s)))
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
	(defmacro defcvar (&rest b) (static-qq (def *compile_env* ~b)))
	(defmacro deffvar (&rest b) (static-qq (def *func_env* ~b)))
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
