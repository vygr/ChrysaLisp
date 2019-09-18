;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defcvar '*emit-buffer* (list) '*distance* (list) '*pc* nil '*pass* nil
	'*out-buffer* (cat "") '*old-out-buffer* (cat "") '*out-stream* nil)

(defmacro-bind emit (&rest _)
	`(push *emit-buffer* ~_))

(defcfun-bind emit-reset ()
	(push (clear *distance* *emit-buffer*) progn))

(defcfun-bind emit-passes ()
	(defq b nil)
	(setq *pass* 0 *pc* 0 *out-stream* (string-stream *out-buffer*))
	(eval (bind-fun *emit-buffer*) *compile-env*)
	(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))
	(until (eql *out-buffer* *old-out-buffer*)
		(setq b *old-out-buffer* *old-out-buffer* *out-buffer* *out-stream* (string-stream b) *pc* 0)
		(eval *emit-buffer* *compile-env*)
		(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))))

(defcfun print-emit (_)
	(and (lst? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defcfun-bind emit-label (_)
	(set (env) _ *pc*))

(defcfun-bind emit-byte (&rest _)
	(write-char *out-stream* _ 1)
	(setq *pc* (+ *pc* (length _))))

(defcfun-bind emit-short (&rest _)
	(write-char *out-stream* _ 2)
	(setq *pc* (+ *pc* (* (length _) 2))))

(defcfun-bind emit-int (&rest _)
	(write-char *out-stream* _ 4)
	(setq *pc* (+ *pc* (* (length _) 4))))

(defcfun-bind emit-long (&rest _)
	(write-char *out-stream* _ 8)
	(setq *pc* (+ *pc* (* (length _) 8))))

(defcfun-bind emit-string (_)
	(write *out-stream* _)
	(setq *pc* (+ *pc* (length _))))

(defcfun-bind emit-align (a &optional b)
	(when (/= *pass* 0)
		(defq p *pc* b (opt b 0) s *out-stream*)
		(setq *pc* (align p a))
		(while (<= (setq p (inc p)) *pc*)
			(write-char s b))))

;;;;;;;;;;;;;;;;;;;;
; VP level optimizer
;;;;;;;;;;;;;;;;;;;;

(defmacro opt-emit-read-ops ()
	''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir))

(defmacro opt-emit-write-ops ()
	''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri))

(defmacro opt-emit-write-ops1 ()
	''(emit-cpy-ri-i emit-cpy-ri-s emit-cpy-ri-b emit-cpy-ri))

(defmacro opt-emit-read-ops1 ()
	''(emit-cpy-ir-i emit-cpy-ir-s emit-cpy-ir-b emit-cpy-ir))

(defmacro opt-emit-read-ops2 ()
	''(emit-cpy-ir-ui emit-cpy-ir-us emit-cpy-ir-ub emit-cpy-ir))

(defmacro opt-emit-call-ops ()
	''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p))

(defmacro opt-emit-kills-ops ()
	''(emit-beq emit-bne emit-blt emit-ble emit-bgt emit-bge emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p
		emit-alloc emit-free))

(defmacro opt-emit-two-out-ops ()
	''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u))

(defmacro opt-emit-multi-out-ops ()
	''(emit-pop))

(defmacro opt-trashes? (r e)
	`(cond
		((eql ,r (elem -2 ,e)) t)
		((find (elem 0 ,e) (opt-emit-two-out-ops)) (eql ,r (elem -3 ,e)))
		((find (elem 0 ,e) (opt-emit-multi-out-ops)) (find ,r ,e))))

(defcfun-bind opt-emit-trashes-without-calls? (r e)
	(opt-trashes? r e))

(defcfun-bind opt-emit-trashes-with-calls? (r e)
	(cond
		((find (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defcfun-bind opt-emit-trashes-with-kills? (r e)
	(cond
		((find (elem 0 e) (opt-emit-kills-ops)) t)
		((find (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defcfun-bind opt-emit-find-rw (k _ r w b i)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(= i (eval (elem 3 e))))
				(setq c 1) _)
			((and (eql r (elem 0 e))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(= i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit-buffer*)))
	(and _ (/= _ -1) _))

(defcfun-bind opt-emit-find-rw1 (k _ ra rb w b i)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(= i (eval (elem 3 e))))
				(setq c 1) _)
			((and (or (eql ra (elem 0 e)) (eql rb (elem 0 e)))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(= i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit-buffer*)))
	(and _ (/= _ -1) _))

(defcfun-bind opt-emit-find-rr (k _ rs rd)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((match? e `(emit-cpy-rr ,rs ,rd)) _)
			((opt-trashes? rs e) -1)
			((k rd e) -1))) (list *emit-buffer*)))
	(and _ (/= _ -1) _))

(defcfun-bind opt-emit-buffer ()
	(each! 2 -1 (lambda (e)
		(cond
			;read after read/write
			((defq c (find (defq o (elem 0 e)) (opt-emit-read-ops)))
				(when (defq w (opt-emit-find-rw opt-emit-trashes-with-calls? _
								(elem c (opt-emit-read-ops))
								(elem c (opt-emit-write-ops))
								(elem 1 e) (eval (elem 2 e))))
					(defq r (elem c (elem w *emit-buffer*)))
					(when (not (some! (inc w) _ t (lambda (_)
							(opt-emit-trashes-without-calls? r _)) (list *emit-buffer*)))
						(elem-set _ *emit-buffer*
							(if (eql r (elem 3 e))
								(lambda)
								`(emit-cpy-rr ,r ,(elem 3 e)))))))
			;write after write
			((defq c (find o (opt-emit-write-ops1)))
				(when (defq w (opt-emit-find-rw1 opt-emit-trashes-with-kills? _
								(elem c (opt-emit-read-ops1))
								(elem c (opt-emit-read-ops2))
								(elem c (opt-emit-write-ops1))
								(elem 2 e) (eval (elem 3 e))))
					(when (= c 1)
						(elem-set w *emit-buffer* (lambda)))))
			;redundant copies
			((eql o 'emit-cpy-rr)
				(when (opt-emit-find-rr opt-emit-trashes-with-calls? _ (elem 2 e) (elem 1 e))
					(elem-set _ *emit-buffer* (lambda))))
			))
	(list *emit-buffer*)))

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(each (lambda (_)
	(defcvar _ _)) '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 rsp))

(defcmacro vp-def (_ &optional l)
	(defq v '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(setq l (apply cat (map (lambda (x y)
		(list (list 'quote x) y)) _ (if l (merge (cat (eval l)) v) v))))
	`(progn (defcvar ~l) (apply push (cat (list *func-syms*) ',_))))

(defmacro type-sym (_)
	`(sym (str "_t_" ,_)))

(defcfun-bind vp-type-t (_)
	(if (setq _ (find (elem 0 _) "bBsSiI"))
		(elem _ '(b ub s us i ui)) ""))

(defcfun-bind vp-type (o)
	(defq i (type-of o))
	(cond
		((= i lisp_type_sym)
			(cond
				((reg o) 'r)
				(t 'c)))
		((= i lisp_type_int) 'c)
		((= i lisp_type_lst)
			(setq i (elem 0 o))
			(cond
				((reg i)
					(setq i (elem 1 o))
					(cond
						((reg i)
							(if (= 2 (length o)) 'd
								(sym (cat 'd (elem 2 o)))))
						((/= 2 (length o))
							(sym (cat 'i (elem 2 o))))
						((num? i) 'i)
						((setq i (def? (type-sym i)))
							(sym (cat 'i (vp-type-t i))))
						(t 'i)))
				((eql i '@) i)
				((eql i '$) i)
				((eql i '&)
					(if (reg (elem 1 o))
						(if (reg (elem 2 o)) '&d '&i) nil))
				(t 'c)))
		((= i lisp_type_str) 's)
		(t nil)))

(defcfun-bind label-sym (s) (sym (cat "_l_" s)))
(defcfun-bind vp-label (l) (setq l (label-sym l))
	(emit (list 'emit-label (list 'quote l))) (push *func-syms* l) (defcvar l 0))
(defcfun-bind vp-align (a &optional b) (emit (list 'emit-align a b)))
(defcfun-bind vp-string (s) (emit (list 'emit-string s)))
(defcfun-bind vp-byte (&rest b) (emit (cat (list 'emit-byte) b)))
(defcfun-bind vp-short (&rest b) (emit (cat (list 'emit-short) b)))
(defcfun-bind vp-int (&rest b) (emit (cat (list 'emit-int) b)))
(defcfun-bind vp-long (&rest b) (emit (cat (list 'emit-long) b)))

(defmacro vp-bcr (_)
	`(defcfun-bind ,(sym (cat "vp-" _ "-cr")) (c d l)
		(emit (list 'emit-cmp-cr c d)
			(list ',(sym (cat "emit-" _)) (label-sym l) (dec (length (push *distance* 0)))))))
(defmacro vp-brr (_)
	`(defcfun-bind ,(sym (cat "vp-" _ "-rr")) (s d l)
		(emit (list 'emit-cmp-rr s d)
			(list ',(sym (cat "emit-" _)) (label-sym l) (dec (length (push *distance* 0)))))))
(defmacro vp-op-cr (_)
	`(defcfun-bind ,(sym (cat "vp-" _ "-cr")) (c d) (emit (list ',(sym (cat "emit-" _ "-cr")) c d))))
(defmacro vp-op-rr (_)
	`(defcfun-bind ,(sym (cat "vp-" _ "-rr")) (s d) (emit (list ',(sym (cat "emit-" _ "-rr")) s d))))
(defmacro vp-mem (_)
	`(defcfun-bind ,(sym (cat "vp-cpy-" _)) (x y z) (emit (list ',(sym (cat "emit-cpy-" _)) x y z))))

(defcfun vp-push (&rest b) (emit (cat (list 'emit-push) b)))
(defcfun vp-pop (&rest b) (emit (cat (list 'emit-pop) b)))
(defcfun vp-alloc (c) (emit (list 'emit-alloc c)))
(defcfun vp-free (c) (emit (list 'emit-free c)))
(defcfun vp-ret () (emit (list 'emit-ret)))

(defcfun vp-call-abi (r b i x) (emit (cat `(emit-call-abi ,r ,b ,i) x)))
(defcfun vp-call (l) (emit (list 'emit-call (label-sym l))))
(defcfun vp-call-r (d) (emit (list 'emit-call-r d)))
(defcfun vp-call-i (b i) (emit (list 'emit-call-i b i)))
(defcfun-bind vp-call-p (l) (emit (list 'emit-call-p (label-sym l))))

(defcfun vp-jmp (l) (emit (list 'emit-jmp (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-jmp-r (d) (emit (list 'emit-jmp-r d)))
(defcfun vp-jmp-i (b i) (emit (list 'emit-jmp-i b i)))
(defcfun vp-jmp-p (l) (emit (list 'emit-jmp-p (label-sym l))))

(defcfun vp-lea-i (b i d) (emit (list 'emit-lea-i b i d)))
(defcfun vp-lea-d (b i d) (emit (list 'emit-lea-d b i d)))
(defcfun vp-lea-p (l r) (emit (list 'emit-lea-p (label-sym l) r)))
(defcfun vp-cpy-pr (l d) (emit (list 'emit-cpy-pr (label-sym l) d)))
(defcfun vp-cpy-rp (s l) (emit (list 'emit-cpy-rp s (label-sym l))))

(defcfun vp-div-rrr (d r q) (emit (list 'emit-div-rrr d r q)))
(defcfun vp-div-rrr-u (d r q) (emit (list 'emit-div-rrr-u d r q)))

(vp-bcr beq) (vp-bcr bne) (vp-bcr blt) (vp-bcr bgt) (vp-bcr ble) (vp-bcr bge)
(vp-brr beq) (vp-brr bne) (vp-brr blt) (vp-brr bgt) (vp-brr ble) (vp-brr bge)
(vp-op-cr seq) (vp-op-cr sne) (vp-op-cr slt) (vp-op-cr sgt) (vp-op-cr sle) (vp-op-cr sge)
(vp-op-rr seq) (vp-op-rr sne) (vp-op-rr slt) (vp-op-rr sgt) (vp-op-rr sle) (vp-op-rr sge)

(vp-mem dr-b) (vp-mem dr-i) (vp-mem dr-s) (vp-mem dr-ub) (vp-mem dr-ui)
(vp-mem dr-us) (vp-mem dr) (vp-mem ir-b) (vp-mem ir-i) (vp-mem ir-s)
(vp-mem ir-ub) (vp-mem ir-ui) (vp-mem ir-us) (vp-mem ir)

(vp-mem rd-b) (vp-mem rd-i) (vp-mem rd-s) (vp-mem rd) (vp-mem ri-b)
(vp-mem ri-i) (vp-mem ri-s) (vp-mem ri)

(vp-op-cr cpy) (vp-op-cr add) (vp-op-cr sub) (vp-op-cr mul) (vp-op-cr and)
(vp-op-cr or) (vp-op-cr xor) (vp-op-cr shl) (vp-op-cr shr) (vp-op-cr asr)
(vp-op-rr cpy) (vp-op-rr add) (vp-op-rr sub) (vp-op-rr mul) (vp-op-rr and)
(vp-op-rr or) (vp-op-rr xor) (vp-op-rr shl) (vp-op-rr shr) (vp-op-rr asr)
(vp-op-rr lnot) (vp-op-rr land) (vp-op-rr swp) (vp-op-rr ext)

;;;;;;;;;;;;;;;;;;;;;
; Arch Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(case *cpu*
(x86_64 (include 'sys/x64.inc))
(aarch64 (include 'sys/arm.inc))
(t (throw "No such CPU !" *cpu*)))
