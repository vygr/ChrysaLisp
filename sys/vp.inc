;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defmacro emit (&rest _)
	`(push *emit_buffer* ~_))

(defun emit-reset ()
	(defcvar '*emit_buffer* (push (clear '()) progn) '*distance* (clear '())
		'*out_buffer* "" '*old_out_buffer* ""))

(defun emit-passes ()
	(defcvar '*pass* 0 '*pc* 0 '*out_stream* (string-stream *out_buffer*))
	(defq b nil)
	(eval (prebind *emit_buffer*) *compile_env*)
	(setq *out_buffer* (str *out_stream*) *pass* (inc *pass*))
	(until (eql *out_buffer* *old_out_buffer*)
		(task-sleep 0)
		(setq b *old_out_buffer* *old_out_buffer* *out_buffer* *out_stream* (string-stream b) *pc* 0)
		(eval *emit_buffer* *compile_env*)
		(setq *out_buffer* (str *out_stream*) *pass* (inc *pass*))))

(defun print-emit (_)
	(and (list? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defun emit-label (_)
	(set (penv) _ *pc*))

(defun emit-byte (&rest _)
	(write-char *out_stream* _ 1)
	(setq *pc* (+ *pc* (length _))))

(defun emit-short (&rest _)
	(write-char *out_stream* _ 2)
	(setq *pc* (+ *pc* (* (length _) 2))))

(defun emit-int (&rest _)
	(write-char *out_stream* _ 4)
	(setq *pc* (+ *pc* (* (length _) 4))))

(defun emit-long (&rest _)
	(write-char *out_stream* _ 8)
	(setq *pc* (+ *pc* (* (length _) 8))))

(defun emit-string (_)
	(write *out_stream* _)
	(setq *pc* (+ *pc* (length _))))

(defun emit-align (a &optional b)
	(when (/= *pass* 0)
		(defq p *pc* b (opt b 0) s *out_stream*)
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

(defmacro opt-emit-read-kills-ops ()
	''(emit-label emit-call emit-call-abi emit-call-i emit-call-r emit-call-p
		emit-alloc emit-free))

(defmacro opt-emit-kills-ops ()
	''(emit-jmp emit-jmp-i emit-jmp-r emit-jmp-p emit-alloc emit-free emit-cmp-cr emit-cmp-rr))

(defmacro opt-emit-two-out-ops ()
	''(emit-land-rr emit-lnot-rr emit-swp-rr emit-div-rrr emit-div-rrr-u))

(defmacro opt-emit-multi-out-ops ()
	''(emit-pop))

(defmacro opt-trashes? (r e)
	`(cond
		((eql ,r (elem -2 ,e)) t)
		((find-rev (elem 0 ,e) (opt-emit-two-out-ops)) (eql ,r (elem -3 ,e)))
		((find-rev (elem 0 ,e) (opt-emit-multi-out-ops)) (find-rev ,r ,e))))

(defun opt-emit-trashes? (r e)
	(opt-trashes? r e))

(defun opt-emit-trashes-with-calls? (r e)
	(cond
		((find-rev (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defun opt-emit-trashes-reads? (r e)
	(cond
		((find-rev (elem 0 e) (opt-emit-read-kills-ops)) t)
		(t (opt-trashes? r e))))

(defun opt-emit-trashes-with-kills? (r e)
	(cond
		((find-rev (elem 0 e) (opt-emit-kills-ops)) t)
		((find-rev (elem 0 e) (opt-emit-call-ops)) t)
		(t (opt-trashes? r e))))

(defun opt-emit-find-rw (k _ r w b i)
	(defq _ (some! _ 0 nil (lambda (e)
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
			((k b e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-find-rw1 (k _ ra rb w b i)
	(defq _ (some! _ 0 nil (lambda (e)
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
			((k b e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-find-rr (k _ rs rd)
	(defq _ (some! _ 0 nil (lambda (e)
		(cond
			((match? e `(emit-cpy-rr ,rs ,rd)) _)
			((opt-trashes? rs e) -1)
			((k rd e) -1))) (list *emit_buffer*)))
	(and _ (/= _ -1) _))

(defun opt-emit-buffer ()
	(each! 2 -1 (lambda (e)
		(cond
			;read after read/write
			((defq c (find-rev (defq o (elem 0 e)) (opt-emit-read-ops)))
				(when (defq w (opt-emit-find-rw opt-emit-trashes-reads? _
								(elem c (opt-emit-read-ops))
								(elem c (opt-emit-write-ops))
								(elem 1 e) (eval (elem 2 e))))
					(defq r (elem c (elem w *emit_buffer*)))
					(when (not (some! (inc w) _ nil (lambda (_)
							(opt-emit-trashes? r _)) (list *emit_buffer*)))
						(elem-set _ *emit_buffer*
							(if (eql r (elem 3 e))
								(lambda)
								`(emit-cpy-rr ,r ,(elem 3 e)))))))
			;write after write
			((defq c (find-rev o (opt-emit-write-ops1)))
				(when (defq w (opt-emit-find-rw1 opt-emit-trashes-with-kills? _
								(elem c (opt-emit-read-ops1))
								(elem c (opt-emit-read-ops2))
								(elem c (opt-emit-write-ops1))
								(elem 2 e) (eval (elem 3 e))))
					(when (= c 1)
						(elem-set w *emit_buffer* (lambda)))))
			;redundant copies
			((eql o 'emit-cpy-rr)
				(when (opt-emit-find-rr opt-emit-trashes-with-calls? _ (elem 2 e) (elem 1 e))
					(elem-set _ *emit_buffer* (lambda))))
			))
	(list *emit_buffer*)))

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(each (# (defcvar %0 %0)) '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 rsp))

(defmacro vp-def (_ &optional l)
	(defq v '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(setq l (apply cat (map (lambda (x y)
		(list (list 'quote x) y)) _ (if l (merge-obj (cat (eval l)) v) v))))
	`(progn (defcvar ~l) (apply push (cat (list *func_syms*) ',_))))

(defmacro type-sym (_)
	`(sym (str "_t_" ,_)))

(defun vp-type-t (_)
	(if (setq _ (find-rev (elem 0 _) "bBsSiI"))
		(elem _ '(b ub s us i ui)) ""))

(defun vp-type (o)
	(case (pop (type-of o))
		(:sym
			(cond
				((reg o) :r)
				(t :c)))
		(:num :c)
		(:list
			(defq i (elem 0 o))
			(cond
				((reg i)
					(setq i (elem 1 o))
					(cond
						((reg i)
							(if (= 2 (length o)) :d
								(sym (cat :d (elem 2 o)))))
						((/= 2 (length o))
							(sym (cat :i (elem 2 o))))
						((num? i) :i)
						((setq i (get (type-sym i)))
							(sym (cat :i (vp-type-t i))))
						(t :i)))
				((eql i '@) :@)
				((eql i '$) :$)
				((eql i '&)
					(if (reg (elem 1 o))
						(if (reg (elem 2 o)) :&d :&i) nil))
				(t :c)))
		(:str :s)
		(t nil)))

(defun label-sym (s) (sym (cat "_l_" s)))
(defun vp-label (l) (setq l (label-sym l))
	(emit (list 'emit-label (list 'quote l))) (push *func_syms* l) (defcvar l 0))
(defun vp-align (a &optional b) (emit (list 'emit-align a b)))
(defun vp-string (s) (emit (list 'emit-string s)))
(defun vp-byte (&rest b) (emit (cat (list 'emit-byte) b)))
(defun vp-short (&rest b) (emit (cat (list 'emit-short) b)))
(defun vp-int (&rest b) (emit (cat (list 'emit-int) b)))
(defun vp-long (&rest b) (emit (cat (list 'emit-long) b)))

(defmacro vp-bcr (_)
	`(defun ,(sym (cat "vp-" _ "-cr")) (c d l)
		(emit (list 'emit-cmp-cr c d)
			(list ',(sym (cat "emit-" _)) (label-sym l) (dec (length (push *distance* 0)))))))
(defmacro vp-brr (_)
	`(defun ,(sym (cat "vp-" _ "-rr")) (s d l)
		(emit (list 'emit-cmp-rr s d)
			(list ',(sym (cat "emit-" _)) (label-sym l) (dec (length (push *distance* 0)))))))
(defmacro vp-op-cr (_)
	`(defun ,(sym (cat "vp-" _ "-cr")) (c d) (emit (list ',(sym (cat "emit-" _ "-cr")) c d))))
(defmacro vp-op-rr (_)
	`(defun ,(sym (cat "vp-" _ "-rr")) (s d) (emit (list ',(sym (cat "emit-" _ "-rr")) s d))))
(defmacro vp-mem (_)
	`(defun ,(sym (cat "vp-cpy-" _)) (x y z) (emit (list ',(sym (cat "emit-cpy-" _)) x y z))))

(defun vp-push (&rest b) (emit (cat (list 'emit-push) b)))
(defun vp-pop (&rest b) (emit (cat (list 'emit-pop) b)))
(defun vp-alloc (c) (emit (list 'emit-alloc c)))
(defun vp-free (c) (emit (list 'emit-free c)))
(defun vp-ret () (emit (list 'emit-ret)))
(defun vp-brk (&optional n) (emit (list 'emit-brk (opt n 0))))

(defun vp-call-abi (r b i n x) (emit (cat (list 'emit-call-abi r b i n) x)))
(defun vp-call (l) (emit (list 'emit-call (label-sym l))))
(defun vp-call-r (d) (emit (list 'emit-call-r d)))
(defun vp-call-i (b i) (emit (list 'emit-call-i b i)))
(defun vp-call-p (l) (emit (list 'emit-call-p (label-sym l))))

(defun vp-jmp (l) (emit (list 'emit-jmp (label-sym l) (dec (length (push *distance* 0))))))
(defun vp-jmp-r (d) (emit (list 'emit-jmp-r d)))
(defun vp-jmp-i (b i) (emit (list 'emit-jmp-i b i)))
(defun vp-jmp-p (l) (emit (list 'emit-jmp-p (label-sym l))))

(defun vp-lea-i (b i d) (emit (list 'emit-lea-i b i d)))
(defun vp-lea-d (b i d) (emit (list 'emit-lea-d b i d)))
(defun vp-lea-p (l r) (emit (list 'emit-lea-p (label-sym l) r)))
(defun vp-cpy-pr (l d) (emit (list 'emit-cpy-pr (label-sym l) d)))

(defun vp-div-rrr (d r q) (emit (list 'emit-div-rrr d r q)))
(defun vp-div-rrr-u (d r q) (emit (list 'emit-div-rrr-u d r q)))

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
(x86_64 (include "sys/x64.inc"))
(aarch64 (include "sys/arm64.inc"))
(vp64 (include "sys/vp64.inc"))
(t (throw "No such CPU !" *cpu*)))
