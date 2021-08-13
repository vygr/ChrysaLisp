;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defmacro emit (&rest _)
	`(push *emit_list* ~_))

(defun emit-reset ()
	(defcvar '*emit_list* (push (clear '()) progn) '*distance* (clear '())
		'*out_buffer* "" '*old_out_buffer* ""))

(defun emit-passes ()
	(defcvar '*pass* 0 '*pc* 0 '*out_stream* (string-stream *out_buffer*))
	(defq b nil)
	(eval (prebind *emit_list*) *compile_env*)
	(setq *out_buffer* (str *out_stream*) *pass* (inc *pass*))
	(until (eql *out_buffer* *old_out_buffer*)
		(task-slice)
		(setq b *old_out_buffer* *old_out_buffer* *out_buffer* *out_stream* (string-stream b) *pc* 0)
		(eval *emit_list* *compile_env*)
		(setq *out_buffer* (str *out_stream*) *pass* (inc *pass*))))

(defun print-emit (_)
	(and (list? _) (not (eql (elem 0 _) 'lambda)) (print _)))

(defun emit-label (_)
	(set (penv) _ *pc*))

(defun emit-byte (&rest _)
	(write-char *out_stream* _)
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

;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

(include "lib/asm/vpopt.inc")

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(each (# (defcvar %0 %0)) '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 rsp))

(defmacro vp-def (_ &optional l)
	(if (> (length (defq v (filter (lambda (_)
			(and (def? _ *compile_env*) (not (find _ *func_syms*)))) _))) 0)
		(throw "VP redefine !" v))
	(defq v '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	(setq l (apply cat (map (lambda (x y)
		(list (list 'quote x) y)) _ (if l (merge-obj (cat (eval l)) v) v))))
	`(progn (defcvar ~l) (apply push (cat (list *func_syms*) ',_))))

(defmacro type-sym (_)
	`(sym (str ,_ "_t")))

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
(defun vp-byte (&rest b) (emit (cat '(emit-byte) b)))
(defun vp-short (&rest b) (emit (cat '(emit-short) b)))
(defun vp-int (&rest b) (emit (cat '(emit-int) b)))
(defun vp-long (&rest b) (emit (cat '(emit-long) b)))

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

(defun vp-push (&rest b) (emit (cat '(emit-push) b)))
(defun vp-pop (&rest b) (emit (cat '(emit-pop) b)))
(defun vp-alloc (c) (emit (list 'emit-alloc c)))
(defun vp-free (c) (emit (list 'emit-free c)))
(defun vp-ret () (emit '(emit-ret)))
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
