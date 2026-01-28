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
(ffi "class/fixed/lisp_ceil" ceil)
; (ceil fixed) -> fixed
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
(ffi "class/fixeds/lisp_ceil" fixeds-ceil)
; (fixeds-ceil fixeds [fixeds]) -> fixeds
(ffi "class/real/lisp_quant" quant)
; (quant real tol) -> real
(ffi "class/reals/lisp_quant" reals-quant)
; (reals-quant reals tol [reals]) -> reals

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
(ffi "class/seq/lisp_splice" splice)
; (splice seq1 seq2 idxs) -> seq
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
(ffi "class/str/lisp_compress" compress)
; (compress str tab_width) -> str
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
(ffi "class/str/lisp_split" split)
; (split str [cls]) -> strs
(ffi "class/str/lisp_encode" hex-encode)
; (hex-encode str) -> str
(ffi "class/str/lisp_decode" hex-decode)
; (hex-decode str) -> str
(ffi "class/str/lisp_startw" starts-with)
; (starts-with str str) -> :nil | :t
(ffi "class/str/lisp_endw" ends-with)
; (ends-with str str) -> :nil | :t

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
(ffi "class/hmap/lisp_copy" env-copy)
; (env-copy env num) -> env
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic Object Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "class/obj/lisp_hash" hash)
; (hash obj) -> num
(ffi "class/obj/lisp_type" type-of)
; (type-of obj) -> (... :obj)
(ffi "class/obj/lisp_fget" obj-get)
; (obj-get obj offset type) -> val
(ffi "class/obj/lisp_fset" obj-set)
; (obj-set obj offset type val) -> obj
(ffi "class/obj/lisp_wref" weak-ref)
; (weak-ref obj) -> num
(ffi "class/obj/lisp_oref" obj-ref)
; (obj-ref num) -> obj
(ffi "class/obj/lisp_eql" eql)
; (eql obj obj) -> :nil | :t
(ffi "class/obj/lisp_nql" nql)
; (nql obj obj) -> :nil | :t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Platform Implementation Interface (PII)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
; Kernel & System
;;;;;;;;;;;;;;;;;;;;;;;;;;
(ffi "sys/kernel/lisp_stats" kernel-stats)
; (kernel-stats) -> (task_count mem_used mem_avail max_stack)
(ffi "sys/load/lisp_path" load-path)
; (load-path) -> path

;;;;;;;;;;;;;;;;;;;;
; Task Management
;;;;;;;;;;;;;;;;;;;;
(ffi "sys/task/lisp_mailbox" task-mbox)
; (task-mbox) -> netid
(ffi "sys/task/lisp_count" task-count)
; (task-count bias) -> count
(ffi "sys/task/lisp_sleep" task-sleep)
; (task-sleep usec)
(ffi "sys/task/lisp_timeslice" task-slice)
; (task-slice)

;;;;;;;;;;;;;;;;;;;;;;
; Mail & Messaging
;;;;;;;;;;;;;;;;;;;;;;
(ffi "sys/mail/lisp_netid" mail-mbox)
; (mail-mbox) -> netid
(ffi "sys/mail/lisp_declare" mail-declare)
; (mail-declare mbox name info) -> key
(ffi "sys/mail/lisp_devices" mail-nodes)
; (mail-nodes) -> nodeids
(ffi "sys/mail/lisp_enquire" mail-enquire)
; (mail-enquire prefix) -> netids
(ffi "sys/mail/lisp_forget" mail-forget)
; (mail-forget key)
(ffi "sys/mail/lisp_poll" mail-poll)
; (mail-poll mboxs) -> :nil | idx
(ffi "sys/mail/lisp_validate" mail-validate)
; (mail-validate mbox) -> :t | :nil
(ffi "sys/mail/lisp_read" mail-read)
; (mail-read mbox) -> :nil | msg
(ffi "sys/mail/lisp_select" mail-select)
; (mail-select mboxs) -> idx
(ffi "sys/mail/lisp_send" mail-send)
; (mail-send mbox obj)
(ffi "sys/mail/lisp_timeout" mail-timeout)
; (mail-timeout mbox ns id) -> mbox

(defq *root_env* (env)
	+byte_size 1 +short_size 2 +int_size 4 +long_size 8 +ptr_size 8
	+str_data 24 +mailbox_id_size 8 +node_id_size 16 +net_id_size 24
	+type_ptr 0 +type_pptr -1 +type_byte -2 +type_ubyte -3
	+type_short -4 +type_ushort -5 +type_int -6 +type_uint -7
	+type_long -8 +type_fixed -9 +type_real -10
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

(defmacro macrobind (%0)
	; (macrobind form) -> (prebind (macroexpand form))
	`(prebind (macroexpand ,%0)))

(defun exec (%0)
	; (exec form)
	(eval (macrobind %0)))

(defmacro const (%0)
	; (const form)
	(exec %0))

(defmacro static-q (%0)
	; (static-q form) -> 'form
	;static quoted
	(list (const quote) (macrobind %0)))

(defmacro static-qq (%0)
	; (static-qq form) -> `form
	;static quasi-quoted
	(list (const quasi-quote) (macrobind %0)))

(defmacro static-qqp (%0)
	; (static-qqp form) -> `form
	;static quasi-quoted, prebind only !
	(list (const quasi-quote) (prebind %0)))

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

(defmacro setd (&rest %0)
	; (setd sym val [sym val] ...)
	(reduce! (lambda (l (s v)) (push l s (list (const ifn) s v)))
		(list (partition %0 2)) (list (const setq))))

;;;;;;;;;;;;
; Predicates
;;;;;;;;;;;;

(defun lambda? (%0)
	; (lambda? form) -> :t | :nil
	(cond ((eql %0 (const lambda))) ((eql %0 'lambda))))

(defun macro? (%0)
	; (macro? form) -> :t | :nil
	(cond ((eql %0 (const macro))) ((eql %0 'macro))))

(defun quote? (%0)
	; (quote? form) -> :t | :nil
	(cond ((eql %0 (const quote))) ((eql %0 'quote))))

(defun quasi-quote? (%0)
	; (quasi-quote? form) -> :t | :nil
	(cond ((eql %0 (const quasi-quote))) ((eql %0 'quasi-quote))))

(defun array? (%0)
	; (array? form) -> :t | :nil
	(find :array (type-of %0)))

(defun list? (%0)
	; (list? form) -> :t | :nil
	(rfind :list (type-of %0)))

(defun num? (%0)
	; (num? form) -> :t | :nil
	(find :num (type-of %0)))

(defun fixed? (%0)
	; (fixed? form) -> :t | :nil
	(find :fixed (type-of %0)))

(defun real? (%0)
	; (real? form) -> :t | :nil
	(find :real (type-of %0)))

(defun nums? (%0)
	; (nums? form) -> :t | :nil
	(find :nums (type-of %0)))

(defun fixeds? (%0)
	; (fixeds? form) -> :t | :nil
	(find :fixeds (type-of %0)))

(defun reals? (%0)
	; (reals? form) -> :t | :nil
	(find :reals (type-of %0)))

(defun func? (%0)
	; (func? form) -> :t | :nil
	(find :func (type-of %0)))

(defun str? (%0)
	; (str? form) -> :t | :nil
	(find :str (type-of %0)))

(defun sym? (%0)
	; (sym? form) -> :t | :nil
	(rfind :sym (type-of %0)))

(defun env? (%0)
	; (env? form) -> :t | :nil
	(find :hmap (type-of %0)))

(defun seq? (%0)
	; (seq? form) -> :t | :nil
	(find :seq (type-of %0)))

(defun lambda-func? (%0)
	; (lambda-func? form) -> :t | :nil
	(if (list? %0) (lambda? (first %0))))

(defun macro-func? (%0)
	; (macro-func? form) -> :t | :nil
	(if (list? %0) (macro? (first %0))))

(defun nil? (%0)
	; (nil? o) -> :t | :nil
	(eql %0 :nil))

(defun atom? (%0)
	; (atom? o) -> :t | :nil
	(cond ((sym? %0) (eql (first %0) ":")) ((list? %0) :nil) (:t)))

(defun msafe? (%0)
	; (msafe? o) -> :t | :nil
	(cond ((sym? %0)) ((atom? %0))))

(defun empty? (%0)
	; (empty? form) -> :t | :nil
	(if (seq? %0) (= (length %0) 0) :t))

(defun nempty? (%0)
	; (nempty? form) -> :t | :nil
	(if (seq? %0) (> (length %0) 0)))

(defun lisp-node? (%0)
	; (lisp_node? node) -> :t | :nil
	(= (logand (code (first %0)) 1) 1))

(defun cpp-node? (%0)
	; (cpp_node? node) -> :t | :nil
	(= (logand (code (first %0)) 1) 0))

;;;;;;;;;;;;;;
; Control flow
;;;;;;;;;;;;;;

(defmacro inc (%0)
	; (inc num) -> num
	(static-qq (+ ,%0 1)))

(defmacro dec (%0)
	; (dec num) -> num
	(static-qq (- ,%0 1)))

(defmacro ++ (%0 &optional i)
	; (++ num [num]) -> num
	(static-qq (setq ,%0 (+ ,%0 ,(ifn i 1)))))

(defmacro -- (%0 &optional i)
	; (-- num [num]) -> num
	(static-qq (setq ,%0 (- ,%0 ,(ifn i 1)))))

(defmacro not (%0)
	; (not form) -> :t | :nil
	(static-qq (if ,%0 :nil :t)))

(defmacro when (x &rest %1)
	; (when tst body)
	(if (= (length %1) 1)
		(static-qq (if ,x ~%1))
		(static-qq (cond (,x ~%1)))))

(defmacro unless (x &rest %1)
	; (unless tst body)
	(if (= (length %1) 1)
		(static-qq (ifn ,x ~%1))
		(static-qq (condn (,x ~%1)))))

(defmacro or (&rest %0)
	; (or [tst] ...) -> :nil | tst
	(map! (const list) (list %0) (list (const cond))))

(defmacro and (&rest %0)
	; (and [tst] ...) -> :t | :nil | tst
	(map! (const list) (list %0) (list (const condn))))

(defmacro times (c &rest %1)
	; (times num body)
	(static-qq (progn (defq ,(defq _c (gensym)) ,c)
		(while (<= 0 (setq ,_c (dec ,_c))) ~%1))))

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

(defun swap (%0 %1 %2)
	; (swap list idx idx)
	(when (/= %1 %2)
		(defq _t (elem-get %0 %1))
		(elem-set (elem-set %0 %1 (elem-get %0 %2)) %2 _t)))

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

(defmacro each (f &rest %1)
	; (each lambda seq ...)
	(static-qq (each! ,f (list ~%1))))

(defmacro reach (f &rest %1)
	; (reach lambda seq ...)
	(static-qq (each! ,f (list ~%1) -1 0)))

(defmacro map (f &rest %1)
	; (map lambda seq ...) -> list
	(static-qq (map! ,f (list ~%1))))

(defmacro rmap (f &rest %1)
	; (rmap lambda seq ...) -> list
	(static-qq (map! ,f (list ~%1) (list) -1 0)))

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

(defmacro let (l &rest %1)
	; (let ([(sym val) ...]) body)
	(static-qq ((lambda ,(map (const first) l) ~%1) ~(map (const second) l))))

(defmacro let* (l &rest %1)
	; (let* ([(sym val) ...]) body)
	(static-qq ((lambda () ,(reduce (lambda (l (var val)) (push l var val)) l (list defq)) ~%1))))

;;;;;;;;;;;;;;;;;;;;;
; Sequence predicates
;;;;;;;;;;;;;;;;;;;;;

(defmacro some (f &rest %1)
	; (some lambda seq ...) -> :nil | form
	(static-qq (some! ,f (list ~%1))))

(defmacro rsome (f &rest %1)
	; (rsome lambda seq ...) -> :nil | form
	(static-qq (some! ,f (list ~%1) :nil -1 0)))

(defmacro every (f &rest %1)
	; (every lambda seq ...) -> :nil | form
	(static-qq (some! ,f (list ~%1) :t)))

(defmacro notany (f &rest %1)
	; (notany lambda seq ...) -> :t | :nil
	(static-qq (not (some! ,f (list ~%1)))))

(defmacro notevery (f &rest %1)
	; (notevery lambda seq ...) -> :t | :nil
	(static-qq (not (some! ,f (list ~%1) :t))))

;;;;;;;;;;;
; Sequences
;;;;;;;;;;;

(defmacro erase (s b e)
	; (erase seq start end) -> seq
	(if (msafe? s)
		(static-qq (splice ,s ,s (nums 0 ,b ,e -1)))
		(static-qq (splice (defq ,(defq _s (gensym)) ,s) ,_s (nums 0 ,b ,e -1)))))

(defmacro insert (s p i)
	; (insert seq pos seq) -> seq
	(if (msafe? p)
		(static-qq (splice ,s ,i (nums 0 ,p 0 -1 ,p -1)))
		(static-qq (splice ,s ,i (nums 0 (defq ,(defq _p (gensym)) ,p) 0 -1 ,_p -1)))))

(defmacro replace (s b e i)
	; (replace seq start end seq) -> seq
	(static-qq (splice ,s ,i (nums 0 ,b 0 -1 ,e -1))))

(defmacro rotate (s i j k)
	; (rotate seq start mid end) -> seq
	(if (and (msafe? s) (msafe? i) (msafe? j) (msafe? k))
		(static-qq (splice ,s ,s (nums 0 ,i ,j ,k ,i ,j ,k -1)))
		(progn
			(defq _s (gensym) _i  (gensym) _j (gensym) _k (gensym))
			(static-qq (splice (defq ,_s ,s) ,_s
				(nums 0 (defq ,_i ,i) (defq ,_j ,j) (defq ,_k ,k) ,_i ,_j ,_k -1))))))

(defun lists (n)
	; (lists n) -> ((list0) ... (listn-1))
	(defq out (cap n (list)))
	(while (>= (-- n) 0) (push out (list)))
	out)

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

(defun neg? (%0)
	; (neg? num) -> :t | :nil
	(< %0 0))

(defun pos? (%0)
	; (pos? num) -> :t | :nil
	(> %0 0))

(defun odd? (%0)
	; (odd? num) -> :t | :nil
	(= 1 (logand 1 %0)))

(defun even? (%0)
	; (even? num) -> :t | :nil
	(= 0 (logand 1 %0)))

(defun lognot (%0)
	; (lognot num) -> num
	(logxor %0 -1))

(defun log2 (%0)
	; (log2 num) -> num
	(when (and (not (= 0 %0)) (= %0 (logand %0 (neg %0))))
		(defq i 0)
		(while (/= 0 (setq %0 (>> %0 1)))
			(++ i)) i))

(defun pow (base exponent)
	; (pow base exponent) -> integer
	(defq res 1)
	(times exponent (setq res (* res base)))
	res)

(defun ntz (%0)
	; (ntz num) -> num
	(defq n 64)
	(while (/= %0 0)
		(setq n (dec n) %0 (<< %0 1))) n)

(defun nto (%0)
	; (nto num) -> num
	(defq n 64 %0 (lognot %0))
	(while (/= %0 0)
		(setq n (dec n) %0 (<< %0 1))) n)

(defun nlz (%0)
	; (nlz num) -> num
	(defq n 0)
	(while (> %0 0)
		(setq n (inc n) %0 (<< %0 1))) n)

(defun nlo (%0)
	; (nlo num) -> num
	(defq n 0 %0 (lognot %0))
	(while (> %0 0)
		(setq n (inc n) %0 (<< %0 1))) n)

;;;;;;;;;;;;;;;;;;
; Fixed point math
;;;;;;;;;;;;;;;;;;

(defq +fp_shift 16 +fp_int_mask (<< -1 +fp_shift) +fp_frac_mask (lognot +fp_int_mask)
	+fp_2pi 6.283185 +fp_pi 3.141592 +fp_hpi 1.570796 +fp_rpi 0.318309)

;;;;;;;;;;;
; Utilities
;;;;;;;;;;;

(defun reflow (words line_width)
	; (reflow words line_width) -> lines
	(defq cnt 0 line (list) lines (list))
	(each (lambda (word)
		(cond
			((> (setq cnt (+ cnt (length word) 1)) line_width)
				(task-slice)
				(push lines (join line " "))
				(push (clear line) word)
				(setq cnt (+ (length word) 1)))
			(:t (push line word)))) words)
	(if (> (length line) 0)
		(push lines (join line " ")) lines))

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

(defmacro ascii-code (%0)
	; (ascii-code char) -> num
	(num-intern (code %0)))

(defmacro ascii-char (%0)
	; (ascii-char num) -> char
	(char (eval %0)))

(defun ascii-upper (%0)
	; (ascii-upper num) -> num
	(if (<= (ascii-code "a") %0 (ascii-code "z"))
		(- %0 (const (num-intern (- (ascii-code "a") (ascii-code "A"))))) %0))

(defun ascii-lower (%0)
	; (ascii-lower num) -> num
	(if (<= (ascii-code "A") %0 (ascii-code "Z"))
		(+ %0 (const (num-intern (- (ascii-code "a") (ascii-code "A"))))) %0))

(defun to-upper (%0)
	; (to-upper str) -> str
	(if (eql %0 "") ""
		(apply (const cat) (map (# (char (ascii-upper (code %0)))) %0))))

(defun to-lower (%0)
	; (to-lower str) -> str
	(if (eql %0 "") ""
		(apply (const cat) (map (# (char (ascii-lower (code %0)))) %0))))

(defun align (num div)
	; (align num div) -> num
	(if (= (% num div) 0) num
		(+ (* (/ num div) div) div)))

(defun str-as-num (%0)
	; (str-as-num str) -> num
	(pop (nums (str-to-num %0))))

(defun num-to-utf8 (%0)
	; (num-to-utf8 num) -> str
	(cond
		((>= %0 0x10000)
			(char (+ 0x808080f0 (>> %0 18) (logand (>> %0 4) 0x3f00)
				(logand (<< %0 10) 0x3f0000) (logand (<< %0 24) 0x3f000000)) 4))
		((>= %0 0x800)
			(char (+ 0x8080e0 (>> %0 12) (logand (<< %0 2) 0x3f00)
				(logand (<< %0 16) 0x3f0000)) 3))
		((>= %0 0x80)
			(char (+ 0x80c0 (>> %0 6) (logand (<< %0 8) 0x3f00)) 2))
		(:t (char %0))))

(defun byte-to-hex-str (%0)
	; (byte-to-hex-str num) -> str
	(hex-encode (char %0)))

(defun short-to-hex-str (%0)
	; (short-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char %0 +short_size))))))

(defun int-to-hex-str (%0)
	; (int-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char %0 +int_size))))))

(defun long-to-hex-str (%0)
	; (long-to-hex-str num) -> str
	(hex-encode (apply (const cat) (reverse (partition (char %0 +long_size))))))

(defun trim-start (s &optional cls)
	; (trim-start str [cls]) -> str
	(slice s (bskip (ifn cls " ") s 0) -1))

(defun trim-end (s &optional cls)
	; (trim-end str [cls]) -> str
	(slice s 0 (rbskip (ifn cls " ") s -1)))

(defun trim (s &optional cls)
	; (trim str [cls]) -> str
	(slice s (bskip (setd cls " ") s 0) (rbskip cls s -1)))

(defun pad (v c &optional f)
	; (pad form width [str]) -> str
	(defq f (ifn f " ") v (str v) l (length v) c (- (max c l) l))
	(while (> c (length f)) (setq f (cat f f)))
	(cat (slice f 0 c) v))

(defun type-to-size (%0)
	; (type-to-size sym) -> num
	(case %0
		((i ui) +int_size)
		((s us) +short_size)
		((b ub) +byte_size)
		;ptr, long, real
		(+long_size)))

(defun lisp-nodes ()
	; (lisp-nodes) -> nodes
	(filter (const lisp-node?) (mail-nodes)))

(defun max-length (%0)
	; (max-length list) -> max
	(reduce (# (max %0 (length %1))) %0 0))

(defun min-length (%0)
	; (min-length list) -> min
	(if (= (length %0) 0) 0
		(reduce (# (min %0 (length %1))) %0 +max_long)))

(defun time-in-seconds (%0)
	; (time-in-seconds time) -> str
	(str (/ %0 1000000) "." (pad (% %0 1000000) 6 "00000")))

(defq *time_it* "")
(defmacro time-it (heading &rest body)
	; (time-it heading body)
	; print the time for the body
	(defq then (gensym) retval (gensym) tabs "  ")
	`(progn
		(prin *time_it* ,heading)
		(print)
		(setq *time_it* (cat *time_it* ,tabs))
		(defq ,then (pii-time) ,retval (progn ~body))
		(prin (setq *time_it* (slice *time_it* 0 (- -1 ,(length tabs))))
			,heading " " (time-in-seconds (- (pii-time) ,then)))
		(print)
		,retval))

;;;;;;;;;;;;;;;;;;;;;;
; string object fields
;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-ubyte (obj idx)
	; (get-ubyte str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_ubyte)))

(defmacro get-ushort (obj idx)
	; (get-ushort str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_ushort)))

(defmacro get-uint (obj idx)
	; (get-uint str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_uint)))

(defmacro get-long (obj idx)
	; (get-long str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_long)))

(defmacro get-byte (obj idx)
	; (get-byte str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_byte)))

(defmacro get-short (obj idx)
	; (get-short str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_short)))

(defmacro get-int (obj idx)
	; (get-int str idx) -> num
	(static-qq (obj-get ,obj (+ +str_data ,idx) +type_int)))

(defmacro get-str (obj idx bytes)
	; (get-str str idx bytes) -> str
	(static-qq (obj-get ,obj (+ +str_data ,idx) ,bytes)))

(defun get-cstr (obj idx)
	; (get-cstr str idx) -> str
	(slice obj idx (find (const (ascii-char 0)) obj idx)))

(defmacro set-byte (obj idx val)
	; (set-byte str idx num) -> str
	(static-qq (obj-set ,obj (+ +str_data ,idx) +type_byte ,val)))

(defmacro set-short (obj idx val)
	; (set-short str idx num) -> str
	(static-qq (obj-set ,obj (+ +str_data ,idx) +type_short ,val)))

(defmacro set-int (obj idx val)
	; (set-int str idx num) -> str
	(static-qq (obj-set ,obj (+ +str_data ,idx) +type_int ,val)))

(defmacro set-long (obj idx val)
	; (set-long str idx num) -> str
	(static-qq (obj-set ,obj (+ +str_data ,idx) +type_long ,val)))

(defmacro set-str (obj idx val)
	; (set-str str idx val) -> str
	(static-qq (obj-set ,obj (+ +str_data ,idx) (length ,val) ,val)))

;;;;;;;;;
; Streams
;;;;;;;;;

(defun age (%0)
	; (age path) -> 0 | time ns
	(if (setq %0 (pii-fstat %0)) (first %0) 0))

(defun load-stream (%0)
	; (load-stream path) -> :nil | stream
	(if (defq %0 (load %0)) (string-stream %0)))

(defun path-to-file ()
	; (path-to-file) -> path
	;the path to this file
	(slice (first (repl-info)) 0 (rfind "/" (first (repl-info)))))

(defun path-to-relative (target &optional current)
	; (path-to-relative target [current]) -> path
	;transform an absolute filename to a relative one
	(setd current (first (repl-info)))
	(defq t_parts (split target "/") c_parts (split current "/")
		i (or (some (# (if (eql %0 %1) :nil (!))) t_parts c_parts)
			(min (length t_parts) (length c_parts)))
		out (list))
	(times (- (length c_parts) i) (push out "."))
	(setq out (cat out (slice t_parts i -1)))
	(if (> (dec (length out)) (length t_parts)) target (join out "/")))

(defun path-to-absolute (target &optional current)
	; (path-to-absolute target [current]) -> path
	;transform a relative filename to an absolute one
	(setd current (first (repl-info)))
	(cond
		((starts-with "." target)
			(defq c_parts (split current "/") p_parts (split target "/")
				i (or (some (# (if (eql %0 ".") :nil (!))) p_parts)
					(length p_parts)))
			(if (> i (length c_parts)) (throw "No relative path !" target))
			(join (cat (slice c_parts 0 (- (length c_parts) i))
					   (slice p_parts i -1)) "/"))
		(target)))

(defun import (module &optional dst_env)
	; (import path [env])
	(if (= (age (setq module (path-to-absolute module))) 0)
		(throw "No such file !" module))
	;does the import already exist ?
	(defq dst_env (ifn dst_env (penv))
		import_sym (sym (cat "*module_" module "*")))
	(unless (get import_sym dst_env)
		;if not existing import it here
		(def dst_env import_sym :t)
		(callback (const repl) dst_env (file-stream module) module)))

(defun import-from (module &optional symbols classes)
	; (import-from [symbols classes])
	(if (= (age (setq module (path-to-absolute module))) 0)
		(throw "No such file !" module))
	(setd symbols '() classes '())
	;import it here, we will discard it on exit
	(repl (file-stream module) module)
	;export only what was requested
	(export-symbols symbols)
	(export-classes classes))

(defmacro read-ubyte (s)
	; (read-ubyte stream) -> num
	(static-qq (read-char ,s)))

(defmacro read-ushort (s)
	; (read-ushort stream) -> num
	(static-qq (read-char ,s ,(num-intern (neg +short_size)))))

(defmacro read-uint (s)
	; (read-uint stream) -> num
	(static-qq (read-char ,s ,(num-intern (neg +int_size)))))

(defmacro read-byte (s)
	; (read-byte stream) -> num
	(static-qq (read-char ,s +byte_size)))

(defmacro read-short (s)
	; (read-short stream) -> num
	(static-qq (read-char ,s +short_size)))

(defmacro read-int (s)
	; (read-int stream) -> num
	(static-qq (read-char ,s +int_size)))

(defmacro read-long (s)
	; (read-long stream) -> num
	(static-qq (read-char ,s +long_size)))

(defmacro write-byte (s n)
	; (write-byte stream list|num) -> bytes
	(static-qq (write-char ,s ,n +byte_size)))

(defmacro write-short (s n)
	; (write-short stream list|num) -> bytes
	(static-qq (write-char ,s ,n +short_size)))

(defmacro write-int (s n)
	; (write-int stream list|num) -> bytes
	(static-qq (write-char ,s ,n +int_size)))

(defmacro write-long (s n)
	; (write-long stream list|num) -> bytes
	(static-qq (write-char ,s ,n +long_size)))

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
		(bind '(o &) (read f))) o)

(defun cpu ()
	; (cpu) -> sym
	(defq o 'x86_64)
	(when (defq f (file-stream 'cpu))
		(bind '(o &) (read f))) o)

(defun abi ()
	; (abi) -> sym
	(defq o 'AMD64)
	(when (defq f (file-stream 'abi))
		(bind '(o &) (read f))) o)

(defun within-compile-env (_f)
	; (within-compile-env lambda)
	(defq *compile_env* (env-resize 1999))
	(defmacro defcvar (&rest b) (static-qq (def *compile_env* ~b)))
	(defmacro deffvar (&rest b) (static-qq (def *func_env* ~b)))
	(defun include (module) (import module *compile_env*))
	(catch (progn (setq _f (_f)) (undef (env) '*func_env* '*compile_env*) _f)
		(progn (undef (penv) '*func_env* '*compile_env*) :nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Boot extension imports
;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/class/struct.inc")
(import "lib/class/class.inc")
(import "lib/collections/collections.inc")
(import "lib/text/searching.inc")
(import "sys/lisp.inc")
(import "class/lisp.inc")

;useful frequency trackers
;; (defq *freq_map* (Fmap 101))

;; (defun freq-update (k)
;; 	(. *freq_map* :update k (# (if %0 (inc %0) 1))) k)

;; (defun freq-print ()
;; 	(defq freq (list))
;; 	(. *freq_map* :each (lambda (k v) (push freq (list k v))))
;; 	(each (lambda ((k v)) (print k " -> " v))
;; 		(reverse (sort freq (# (- (second %0) (second %1)))))))
