;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defcvar '*emit-buffer* (list) '*distance* (list) '*pc* nil '*pass* nil
	'*out-buffer* (cat "") '*old-out-buffer* (cat "") '*out-stream* nil)

(defmacro emit (f)
	`(push *emit-buffer* ,f))

(defcfun emit-reset ()
	(push (clear *distance* *emit-buffer*) progn))

(defcfun emit-passes ()
	(defq b nil)
	(setq *pass* 0 *pc* 0 *out-stream* (string-stream *out-buffer*))
	(eval *emit-buffer* *compile-env*)
	(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))
	(until (eql *out-buffer* *old-out-buffer*)
		(setq b *old-out-buffer* *old-out-buffer* *out-buffer* *out-stream* (string-stream b) *pc* 0)
		(eval *emit-buffer* *compile-env*)
		(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))))

(defcfun print-emit (_)
	(and (lst? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defcfun emit-label (_)
	(set (env) _ *pc*))

(defcfun emit-byte (&rest _)
	(write-char *out-stream* _ 1)
	(setq *pc* (add *pc* (length _))))

(defcfun emit-short (&rest _)
	(write-char *out-stream* _ 2)
	(setq *pc* (add *pc* (mul (length _) 2))))

(defcfun emit-int (&rest _)
	(write-char *out-stream* _ 4)
	(setq *pc* (add *pc* (mul (length _) 4))))

(defcfun emit-long (&rest _)
	(write-char *out-stream* _ 8)
	(setq *pc* (add *pc* (mul (length _) 8))))

(defcfun emit-string (_)
	(write *out-stream* _)
	(setq *pc* (add *pc* (length _))))

(defcfun emit-align (a &optional b)
	(when (ne *pass* 0)
		(defq p *pc* b (opt b 0) s *out-stream*)
		(setq *pc* (align p a))
		(while (le (setq p (inc p)) *pc*)
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

(defcfun opt-emit-trashes-without-calls? (r e)
	(opt-trashes? r e))

(defcfun opt-emit-trashes-with-calls? (r e)
	(cond
		((find (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defcfun opt-emit-trashes-with-kills? (r e)
	(cond
		((find (elem 0 e) (opt-emit-kills-ops)) t)
		((find (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defcfun opt-emit-find-rw (k _ r w b i)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(eq i (eval (elem 3 e))))
				(setq c 1) _)
			((and (eql r (elem 0 e))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(eq i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit-buffer*)))
	(and _ (ne _ -1) _))

(defcfun opt-emit-find-rw1 (k _ ra rb w b i)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((and (eql w (elem 0 e))
				(eql b (elem 2 e))
				(eq i (eval (elem 3 e))))
				(setq c 1) _)
			((and (or (eql ra (elem 0 e)) (eql rb (elem 0 e)))
				(eql b (elem 1 e))
				(not (eql b (elem 3 e)))
				(eq i (eval (elem 2 e))))
				(setq c 3) _)
			((k b e) -1))) (list *emit-buffer*)))
	(and _ (ne _ -1) _))

(defcfun opt-emit-find-rr (k _ rs rd)
	(defq _ (some! _ 0 t (lambda (e)
		(cond
			((match? e (list 'emit-cpy-rr rs rd)) _)
			((opt-trashes? rs e) -1)
			((k rd e) -1))) (list *emit-buffer*)))
	(and _ (ne _ -1) _))

(defcfun opt-emit-buffer ()
	(each! 2 nil nil (lambda (e)
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
								(list 'emit-cpy-rr r (elem 3 e)))))))
			;write after write
			((defq c (find o (opt-emit-write-ops1)))
				(when (defq w (opt-emit-find-rw1 opt-emit-trashes-with-kills? _
								(elem c (opt-emit-read-ops1))
								(elem c (opt-emit-read-ops2))
								(elem c (opt-emit-write-ops1))
								(elem 2 e) (eval (elem 3 e))))
					(when (eq c 1)
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

(defcfun vp-type (o)
	(defq i (type? o))
	(cond
		((eq i lisp_type_sym)
			(cond
				((reg o) 'r)
				(t 'c)))
		((eq i lisp_type_int) 'c)
		((eq i lisp_type_lst)
			(setq i (elem 0 o))
			(cond
				((reg i)
					(if (reg (elem 1 o)) 'd
						(if (eq 2 (length o)) 'i
							(sym (cat "i" (elem 2 o))))))
				((eql i '@) i)
				((eql i '$) i)
				((eql i '&)
					(if (reg (elem 1 o))
						(if (reg (elem 2 o)) '&d '&i) nil))
				(t 'c)))
		((eq i lisp_type_str) 's)
		(t nil)))

(defcfun label-sym (s) (sym (cat "_l_" s)))
(defcfun vp-label (l) (setq l (label-sym l))
	(emit (list 'emit-label (list 'quote l))) (push *func-syms* l) (defcvar l 0))
(defcfun vp-align (a &optional b) (emit (list 'emit-align a b)))
(defcfun vp-string (s) (emit (list 'emit-string s)))
(defcfun vp-byte (&rest b) (emit (cat (list 'emit-byte) b)))
(defcfun vp-short (&rest b) (emit (cat (list 'emit-short) b)))
(defcfun vp-int (&rest b) (emit (cat (list 'emit-int) b)))
(defcfun vp-long (&rest b) (emit (cat (list 'emit-long) b)))

(defcfun vp-beq-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-beq (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bne-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-bne (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-blt-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-blt (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-ble-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-ble (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bgt-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-bgt (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bge-cr (c d l)
	(emit (list 'emit-cmp-cr c d))
	(emit (list 'emit-bge (label-sym l) (dec (length (push *distance* 0))))))

(defcfun vp-beq-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-beq (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bne-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-bne (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-blt-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-blt (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-ble-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-ble (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bgt-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-bgt (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-bge-rr (s d l)
	(emit (list 'emit-cmp-rr s d))
	(emit (list 'emit-bge (label-sym l) (dec (length (push *distance* 0))))))

(defcfun vp-seq-cr (c d) (emit (list 'emit-seq-cr c d)))
(defcfun vp-sne-cr (c d) (emit (list 'emit-sne-cr c d)))
(defcfun vp-slt-cr (c d) (emit (list 'emit-slt-cr c d)))
(defcfun vp-sle-cr (c d) (emit (list 'emit-sle-cr c d)))
(defcfun vp-sgt-cr (c d) (emit (list 'emit-sgt-cr c d)))
(defcfun vp-sge-cr (c d) (emit (list 'emit-sge-cr c d)))

(defcfun vp-seq-rr (s d) (emit (list 'emit-seq-rr s d)))
(defcfun vp-sne-rr (s d) (emit (list 'emit-sne-rr s d)))
(defcfun vp-slt-rr (s d) (emit (list 'emit-slt-rr s d)))
(defcfun vp-sle-rr (s d) (emit (list 'emit-sle-rr s d)))
(defcfun vp-sgt-rr (s d) (emit (list 'emit-sgt-rr s d)))
(defcfun vp-sge-rr (s d) (emit (list 'emit-sge-rr s d)))

(defcfun vp-push (&rest b) (emit (cat (list 'emit-push) b)))
(defcfun vp-pop (&rest b) (emit (cat (list 'emit-pop) b)))

(defcfun vp-call-abi (r b i x) (emit (cat (list 'emit-call-abi r b i) x)))
(defcfun vp-call (l) (emit (list 'emit-call (label-sym l))))
(defcfun vp-call-r (d) (emit (list 'emit-call-r d)))
(defcfun vp-call-i (b i) (emit (list 'emit-call-i b i)))
(defcfun vp-call-p (l) (emit (list 'emit-call-p (label-sym l))))

(defcfun vp-jmp (l) (emit (list 'emit-jmp (label-sym l) (dec (length (push *distance* 0))))))
(defcfun vp-jmp-r (d) (emit (list 'emit-jmp-r d)))
(defcfun vp-jmp-i (b i) (emit (list 'emit-jmp-i b i)))
(defcfun vp-jmp-p (l) (emit (list 'emit-jmp-p (label-sym l))))

(defcfun vp-lea-i (b i d) (emit (list 'emit-lea-i b i d)))
(defcfun vp-lea-d (b i d) (emit (list 'emit-lea-d b i d)))
(defcfun vp-lea-p (l r) (emit (list 'emit-lea-p (label-sym l) r)))

(defcfun vp-cpy-cr (c d) (emit (list 'emit-cpy-cr c d)))
(defcfun vp-cpy-rr (s d) (emit (list 'emit-cpy-rr s d)))
(defcfun vp-cpy-ir (b i d) (emit (list 'emit-cpy-ir b i d)))
(defcfun vp-cpy-dr (b i d) (emit (list 'emit-cpy-dr b i d)))
(defcfun vp-cpy-pr (l d) (emit (list 'emit-cpy-pr (label-sym l) d)))
(defcfun vp-cpy-ri (s b i) (emit (list 'emit-cpy-ri s b i)))
(defcfun vp-cpy-rd (s b i) (emit (list 'emit-cpy-rd s b i)))
(defcfun vp-cpy-rp (s l) (emit (list 'emit-cpy-rp s (label-sym l))))

(defcfun vp-cpy-ir-b (b i d) (emit (list 'emit-cpy-ir-b b i d)))
(defcfun vp-cpy-dr-b (b i d) (emit (list 'emit-cpy-dr-b b i d)))
(defcfun vp-cpy-ir-ub (b i d) (emit (list 'emit-cpy-ir-ub b i d)))
(defcfun vp-cpy-dr-ub (b i d) (emit (list 'emit-cpy-dr-ub b i d)))
(defcfun vp-cpy-ir-s (b i d) (emit (list 'emit-cpy-ir-s b i d)))
(defcfun vp-cpy-dr-s (b i d) (emit (list 'emit-cpy-dr-s b i d)))
(defcfun vp-cpy-ir-us (b i d) (emit (list 'emit-cpy-ir-us b i d)))
(defcfun vp-cpy-dr-us (b i d) (emit (list 'emit-cpy-dr-us b i d)))
(defcfun vp-cpy-ir-i (b i d) (emit (list 'emit-cpy-ir-i b i d)))
(defcfun vp-cpy-dr-i (b i d) (emit (list 'emit-cpy-dr-i b i d)))
(defcfun vp-cpy-ir-ui (b i d) (emit (list 'emit-cpy-ir-ui b i d)))
(defcfun vp-cpy-dr-ui (b i d) (emit (list 'emit-cpy-dr-ui b i d)))

(defcfun vp-cpy-ri-b (s b i) (emit (list 'emit-cpy-ri-b s b i)))
(defcfun vp-cpy-rd-b (s b i) (emit (list 'emit-cpy-rd-b s b i)))
(defcfun vp-cpy-ri-s (s b i) (emit (list 'emit-cpy-ri-s s b i)))
(defcfun vp-cpy-rd-s (s b i) (emit (list 'emit-cpy-rd-s s b i)))
(defcfun vp-cpy-ri-i (s b i) (emit (list 'emit-cpy-ri-i s b i)))
(defcfun vp-cpy-rd-i (s b i) (emit (list 'emit-cpy-rd-i s b i)))

(defcfun vp-add-cr (c d) (emit (list 'emit-add-cr c d)))
(defcfun vp-add-rr (s d) (emit (list 'emit-add-rr s d)))
(defcfun vp-sub-cr (c d) (emit (list 'emit-sub-cr c d)))
(defcfun vp-sub-rr (s d) (emit (list 'emit-sub-rr s d)))
(defcfun vp-mul-cr (c d) (emit (list 'emit-mul-cr c d)))
(defcfun vp-mul-rr (s d) (emit (list 'emit-mul-rr s d)))
(defcfun vp-and-cr (c d) (emit (list 'emit-and-cr c d)))
(defcfun vp-and-rr (s d) (emit (list 'emit-and-rr s d)))
(defcfun vp-or-cr (c d) (emit (list 'emit-or-cr c d)))
(defcfun vp-or-rr (s d) (emit (list 'emit-or-rr s d)))
(defcfun vp-xor-cr (c d) (emit (list 'emit-xor-cr c d)))
(defcfun vp-xor-rr (s d) (emit (list 'emit-xor-rr s d)))

(defcfun vp-lnot-rr (r d) (emit (list 'emit-lnot-rr r d)))
(defcfun vp-land-rr (s d) (emit (list 'emit-land-rr s d)))

(defcfun vp-shl-cr (c d) (emit (list 'emit-shl-cr c d)))
(defcfun vp-shl-rr (s d) (emit (list 'emit-shl-rr s d)))
(defcfun vp-shr-cr (c d) (emit (list 'emit-shr-cr c d)))
(defcfun vp-shr-rr (s d) (emit (list 'emit-shr-rr s d)))
(defcfun vp-asr-cr (c d) (emit (list 'emit-asr-cr c d)))
(defcfun vp-asr-rr (s d) (emit (list 'emit-asr-rr s d)))

(defcfun vp-ret () (emit '(emit-ret)))
(defcfun vp-swp-rr (s d) (emit (list 'emit-swp-rr s d)))
(defcfun vp-ext-rr (s d) (emit (list 'emit-ext-rr s d)))
(defcfun vp-div-rrr (d r q) (emit (list 'emit-div-rrr d r q)))
(defcfun vp-div-rrr-u (d r q) (emit (list 'emit-div-rrr-u d r q)))

(defcfun vp-alloc (c) (emit (list 'emit-alloc c)))
(defcfun vp-free (c) (emit (list 'emit-free c)))

;;;;;;;;;;;;;;;;;;;;;
; Arch Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(case *cpu*
(x86_64 (include 'sys/x64.inc))
(aarch64 (include 'sys/arm.inc))
(otherwise (throw "No such CPU !" *cpu*)))
