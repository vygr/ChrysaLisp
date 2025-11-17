;;;;;;;;;;;;;;
; VP Registers
;;;;;;;;;;;;;;

(defq +vp_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+vp_regs_0_14 ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
(defun vp-reg? (_) (find _ +vp_regs))
(each (# (defcvar %0 %0)) +vp_regs)

(defmacro vp-def (_ &optional l)
	(setq l (apply (const cat) (map (lambda (x y)
		(list (list quote x) y)) _ (if l (merge (cat (eval l)) +vp_regs_0_14) +vp_regs_0_14))))
	(static-qq (progn (deffvar ~l))))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defmacro emit (&rest _) (static-qqp (push *emit_list* ~_)))

(defun emit-vp-code (emit_list)
	;remove nops and rename labels
	(defq out (push (clear '()) 'progn) labels (env 11)
		last_op :nil pc -1 pc_sym :nil)
	;scan the emit list and filter while gathering the label renames
	(each (lambda (inst)
		(cond
			((eql (defq op (first inst)) 'emit-label)
				(unless (eql op last_op)
					(setq pc_sym (sym (str "_" (++ pc))))
					(push out inst))
				(def labels (last (last inst)) pc_sym))
			((eql op 'emit-vp-nop))
			((push out inst)))
		(setq last_op op)) emit_list)
	;scan the tree ! and rename any label symbol elements
	(defq stack (rest out))
	(while (defq lst (pop stack))
		(each! (#
			(if (list? %0)
				(push stack %0)
				(and (sym? %0) (defq %0 (def? %0 labels)) (elem-set lst (!) %0))))
			(list lst) 1))
	out)

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(defun label-sym (s) (sym (cat "_l_" s)))
(defun vp-label (l) (setq l (label-sym l)) (emit `(emit-label ',l)))
(defun vp-align (a &optional b) (if b (emit `(emit-align ,a ,b)) (emit `(emit-align ,a))))
(defun vp-string (s) (emit `(emit-string ,s)))
(defun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defun vp-short (&rest b) (emit `(emit-short ~b)))
(defun vp-int (&rest b) (emit `(emit-int ~b)))
(defun vp-long (&rest b) (emit `(emit-long ~b)))
(defun vp-cstr (s) (vp-string s)(vp-byte 0))

(defun vp-push (&rest b) (emit `(emit-push ~b)))
(defun vp-pop (&rest b) (emit `(emit-pop ~b)))
(defun vp-alloc (c) (emit (static-qqp (emit-alloc ,(eval c (penv))))))
(defun vp-free (c) (emit (static-qqp (emit-free ,(eval c (penv))))))
(defun vp-ret () (emit `(emit-ret)))
(defun vp-brk (&optional n) (emit (static-qqp (emit-brk ,(ifn n 0)))))
(defun vp-sync (&optional n) (emit (static-qqp (emit-sync ,(ifn n 0)))))
(defun vp-stack-init (s f x) (emit `(emit-stack-init ,s ,f ,x)))

(defun vp-call-abi (r b i n x) (emit `(emit-call-abi ,r ,b ,i ,n ~x)))
(defun vp-call (l) (emit `(emit-call ,(label-sym l))))
(defun vp-call-r (d) (emit `(emit-call-r ,d)))
(defun vp-call-i (b i) (emit (static-qqp (emit-call-i ,b ,(eval i (penv))))))
(defun vp-call-p (l) (emit `(emit-call-p ,(label-sym l))))

(defun vp-jmp (l) (emit `(emit-jmp ,(label-sym l))))
(defun vp-jmp-r (d) (emit `(emit-jmp-r ,d)))
(defun vp-jmp-i (b i) (emit (static-qqp (emit-jmp-i ,b ,(eval i (penv))))))
(defun vp-jmp-p (l) (emit `(emit-jmp-p ,(label-sym l))))

(defun vp-lea-i (b i d) (emit (static-qqp (emit-lea-i ,b ,(eval i (penv)) ,d))))
(defun vp-lea-d (b i d) (emit `(emit-lea-d ,b ,i ,d)))
(defun vp-lea-p (l r) (emit `(emit-lea-p ,(label-sym l) ,r)))
(defun vp-cpy-pr (l d) (emit `(emit-cpy-pr ,(label-sym l) ,d)))

(defun vp-div-rrr (d r q) (emit `(emit-div-rrr ,d ,r ,q)))
(defun vp-div-rrr-u (d r q) (emit `(emit-div-rrr-u ,d ,r ,q)))

(defun vp-beq-cr (c d l) (emit (static-qqp (emit-beq-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bne-cr (c d l) (emit (static-qqp (emit-bne-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-blt-cr (c d l) (emit (static-qqp (emit-blt-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bgt-cr (c d l) (emit (static-qqp (emit-bgt-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-ble-cr (c d l) (emit (static-qqp (emit-ble-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bge-cr (c d l) (emit (static-qqp (emit-bge-cr ,(eval c (penv)) ,d ,(label-sym l)))))

(defun vp-beq-rr (s d l) (emit (static-qqp (emit-beq-rr ,s ,d ,(label-sym l)))))
(defun vp-bne-rr (s d l) (emit (static-qqp (emit-bne-rr ,s ,d ,(label-sym l)))))
(defun vp-blt-rr (s d l) (emit (static-qqp (emit-blt-rr ,s ,d ,(label-sym l)))))
(defun vp-bgt-rr (s d l) (emit (static-qqp (emit-bgt-rr ,s ,d ,(label-sym l)))))
(defun vp-ble-rr (s d l) (emit (static-qqp (emit-ble-rr ,s ,d ,(label-sym l)))))
(defun vp-bge-rr (s d l) (emit (static-qqp (emit-bge-rr ,s ,d ,(label-sym l)))))

(defun vp-seq-cr (c d) (emit (static-qqp (emit-seq-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sne-cr (c d) (emit (static-qqp (emit-sne-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-slt-cr (c d) (emit (static-qqp (emit-slt-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sgt-cr (c d) (emit (static-qqp (emit-sgt-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sle-cr (c d) (emit (static-qqp (emit-sle-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sge-cr (c d) (emit (static-qqp (emit-sge-cr ,(logior 0 (eval c (penv))) ,d))))

(defun vp-seq-rr (s d) (emit `(emit-seq-rr ,s ,d)))
(defun vp-sne-rr (s d) (emit `(emit-sne-rr ,s ,d)))
(defun vp-slt-rr (s d) (emit `(emit-slt-rr ,s ,d)))
(defun vp-sgt-rr (s d) (emit `(emit-sgt-rr ,s ,d)))
(defun vp-sle-rr (s d) (emit `(emit-sle-rr ,s ,d)))
(defun vp-sge-rr (s d) (emit `(emit-sge-rr ,s ,d)))

(defun vp-cpy-cr (c d) (emit (static-qqp (emit-cpy-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-add-cr (c d) (emit (static-qqp (emit-add-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sub-cr (c d) (emit (static-qqp (emit-sub-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-mul-cr (c d) (emit (static-qqp (emit-mul-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-and-cr (c d) (emit (static-qqp (emit-and-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-or-cr (c d) (emit (static-qqp (emit-or-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-xor-cr (c d) (emit (static-qqp (emit-xor-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-shl-cr (c d) (emit (static-qqp (emit-shl-cr ,(eval c (penv)) ,d))))
(defun vp-shr-cr (c d) (emit (static-qqp (emit-shr-cr ,(eval c (penv)) ,d))))
(defun vp-asr-cr (c d) (emit (static-qqp (emit-asr-cr ,(eval c (penv)) ,d))))

(defun vp-cpy-rr (s d) (emit `(emit-cpy-rr ,s ,d)))
(defun vp-add-rr (s d) (emit `(emit-add-rr ,s ,d)))
(defun vp-sub-rr (s d) (emit `(emit-sub-rr ,s ,d)))
(defun vp-mul-rr (s d) (emit `(emit-mul-rr ,s ,d)))
(defun vp-and-rr (s d) (emit `(emit-and-rr ,s ,d)))
(defun vp-or-rr (s d) (emit `(emit-or-rr ,s ,d)))
(defun vp-xor-rr (s d) (emit `(emit-xor-rr ,s ,d)))
(defun vp-shl-rr (s d) (emit `(emit-shl-rr ,s ,d)))
(defun vp-shr-rr (s d) (emit `(emit-shr-rr ,s ,d)))
(defun vp-asr-rr (s d) (emit `(emit-asr-rr ,s ,d)))
(defun vp-lnot-rr (s d) (emit `(emit-lnot-rr ,s ,d)))
(defun vp-land-rr (s d) (emit `(emit-land-rr ,s ,d)))
(defun vp-swp-rr (s d) (emit `(emit-swp-rr ,s ,d)))
(defun vp-ext-rr (s d) (emit `(emit-ext-rr ,s ,d)))

(defun vp-cpy-dr-b (b i d) (emit `(emit-cpy-dr-b ,b ,i ,d)))
(defun vp-cpy-dr-s (b i d) (emit `(emit-cpy-dr-s ,b ,i ,d)))
(defun vp-cpy-dr-i (b i d) (emit `(emit-cpy-dr-i ,b ,i ,d)))
(defun vp-cpy-dr-ub (b i d) (emit `(emit-cpy-dr-ub ,b ,i ,d)))
(defun vp-cpy-dr-us (b i d) (emit `(emit-cpy-dr-us ,b ,i ,d)))
(defun vp-cpy-dr-ui (b i d) (emit `(emit-cpy-dr-ui ,b ,i ,d)))
(defun vp-cpy-dr (b i d) (emit `(emit-cpy-dr ,b ,i ,d)))

(defun vp-cpy-rd-b (s b i) (emit `(emit-cpy-rd-b ,s ,b ,i)))
(defun vp-cpy-rd-s (s b i) (emit `(emit-cpy-rd-s ,s ,b ,i)))
(defun vp-cpy-rd-i (s b i) (emit `(emit-cpy-rd-i ,s ,b ,i)))
(defun vp-cpy-rd (s b i) (emit `(emit-cpy-rd ,s ,b ,i)))

(defun vp-cpy-ir-b (b i d) (emit (static-qqp (emit-cpy-ir-b ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir-s (b i d) (emit (static-qqp (emit-cpy-ir-s ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir-i (b i d) (emit (static-qqp (emit-cpy-ir-i ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir-ub (b i d) (emit (static-qqp (emit-cpy-ir-ub ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir-us (b i d) (emit (static-qqp (emit-cpy-ir-us ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir-ui (b i d) (emit (static-qqp (emit-cpy-ir-ui ,b ,(eval i (penv)) ,d))))
(defun vp-cpy-ir (b i d) (emit (static-qqp (emit-cpy-ir ,b ,(eval i (penv)) ,d))))

(defun vp-cpy-ri-b (s b i) (emit (static-qqp (emit-cpy-ri-b ,s ,b ,(eval i (penv))))))
(defun vp-cpy-ri-s (s b i) (emit (static-qqp (emit-cpy-ri-s ,s ,b ,(eval i (penv))))))
(defun vp-cpy-ri-i (s b i) (emit (static-qqp (emit-cpy-ri-i ,s ,b ,(eval i (penv))))))
(defun vp-cpy-ri (s b i) (emit (static-qqp (emit-cpy-ri ,s ,b ,(eval i (penv))))))

(defun vp-min-cr (c d)
	(defq l (label-sym (gensym)))
	(emit (static-qqp (emit-min-cr ,(eval c (penv)) ,d ,l)))
	(emit `(emit-label ',l)))
(defun vp-max-cr (c d)
	(defq l (label-sym (gensym)))
	(emit (static-qqp (emit-max-cr ,(eval c (penv)) ,d ,l)))
	(emit `(emit-label ',l)))
(defun vp-min-rr (s d)
	(defq l (label-sym (gensym)))
	(emit (static-qqp (emit-min-rr ,s ,d ,l)))
	(emit `(emit-label ',l)))
(defun vp-max-rr (s d)
	(defq l (label-sym (gensym)))
	(emit (static-qqp (emit-max-rr ,s ,d ,l)))
	(emit `(emit-label ',l)))
(defun vp-abs-rr (s d)
	(defq l (label-sym (gensym)))
	(emit (static-qqp (emit-abs-rr ,s ,d ,l)))
	(emit `(emit-label ',l)))

;puedo vp instructions
(defun vp-simd (op &rest v)
	(defq max_len (max-length v))
	(each (# (while (< (length %0) max_len) (push %0 (last %0)))) v)
	(each! op v))
