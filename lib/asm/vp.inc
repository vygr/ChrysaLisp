;;;;;;;;;;;;;;
; VP Registers
;;;;;;;;;;;;;;

(import "./regs.inc")
(import "./ops.inc")

(defmacro vp-rdef (%0 &optional %1)
	(setq %1 (if %1 (merge (cat (eval %1)) +vp_rregs_0_14) +vp_rregs_0_14))
	(static-qq (bind ',(push %0 '&ignore) ',%1)))

(defmacro vp-ddef (%0 &optional %1)
	;names of the float registers as doubles, :d0 on, those given first.
	;The same registers as vp-fdef, from the same first one
	(setq %1 (if %1 (merge (cat (eval %1)) +vp_fregs) +vp_fregs))
	(static-qq (bind ',(push %0 '&ignore) ',%1)))

(defmacro vp-fdef (%0 &optional %1)
	;names of the float registers as singles, :f0 on, those given first. A
	;single and its double are one register: with vp-ddef in the same code,
	;as two vp-rdef, one of them is given the registers it is to have
	(setq %1 (if %1 (merge (cat (eval %1)) +vp_sregs) +vp_sregs))
	(static-qq (bind ',(push %0 '&ignore) ',%1)))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defmacro emit (&rest %0) (static-qqp (push *emit_list* ~%0)))

(defun emit-vp-code (emit_list)
	;remove nops and rename labels
	(defq out (list) labels (env 11)
		last_op :nil pc -1 pc_sym :nil)
	;scan the emit list and filter while gathering the label renames
	(each (lambda (inst)
		(cond
			((eql (defq op (first inst)) 'label)
				(unless (eql last_op 'label)
					(setq pc_sym (sym (str "_" (++ pc))))
					(push out inst))
				(def labels (last (last inst)) pc_sym))
			((eql op 'vp-nop))
			((push out inst)))
		(setq last_op op)) emit_list)
	;scan the tree ! and rename any label symbol elements
	(defq stack (cat out))
	(while (defq lst (pop stack))
		(each! (#
			(if (list?? %0)
				(push stack %0)
				(and (sym? %0) (defq %0 (def? %0 labels)) (elem-set lst (!) %0))))
			(list lst) 1))
	out)

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(defun label-sym (s) (sym (cat "_l_" s)))
(defun vp-label (l) (setq l (label-sym l)) (emit `(label ',l)))
(defun vp-align (a &optional b) (if b (emit `(align ,a ,b)) (emit `(align ,a))))
(defun vp-string (s) (emit `(string ,s)))
(defun vp-byte (&rest b) (emit `(byte ~b)))
(defun vp-short (&rest b) (emit `(short ~b)))
(defun vp-int (&rest b) (emit `(int ~b)))
(defun vp-long (&rest b) (emit `(long ~b)))
(defun vp-cstr (s) (vp-string s)(vp-byte 0))

(defun vp-push (&rest b) (emit `(push ~b)))
(defun vp-pop (&rest b) (emit `(pop ~b)))
(defun vp-trash (&rest b) (emit `(trash ~b)))
(defun vp-alloc (c) (emit (static-qqp (alloc ,(eval c (penv))))))
(defun vp-free (c) (emit (static-qqp (free ,(eval c (penv))))))
(defun vp-ret () (emit `(ret)))
(defun vp-sync (&optional n) (emit (static-qqp (sync ,(ifn n 0)))))
(defun vp-stack-init (s f x) (emit `(stack-init ,s ,f ,x)))
;the state a task keeps on its stack as it is switched from, and gets back
;as it is switched to, what the CPU has besides the VP registers. What it
;is, and if there is any, the translator knows, (save-state)
(defun vp-save-state () (emit (list 'save-state)))
(defun vp-load-state () (emit (list 'load-state)))
;the first of the loader, called by the host with n args: they are put in
;:r0 :r1 ... from where the host's ABI has them, and the stack is the VP's.
;The reverse of (vp-call-abi), (entry-abi)
(defun vp-entry-abi (n) (emit (list 'entry-abi n)))

(defun vp-call-abi (r b i n x) (emit `(call-abi ,r ,b ,i ,n ~x)))
(defun vp-call (l) (emit `(call ,(label-sym l))))
(defun vp-call-r (d &optional c m) (emit `(call-r ,d ,c ,m)))
(defun vp-call-rc (b i &optional c m) (emit (static-qqp (call-rc ,b ,(eval i (penv)) ,c ,m))))
(defun vp-call-p (l) (emit `(call-p ,(label-sym l))))

(defun vp-jmp (l) (emit `(jmp ,(label-sym l))))
(defun vp-jmp-r (d &optional c m) (emit `(jmp-r ,d ,c ,m)))
(defun vp-jmp-rc (b i &optional c m) (emit (static-qqp (jmp-rc ,b ,(eval i (penv)) ,c ,m))))
(defun vp-jmp-p (l) (emit `(jmp-p ,(label-sym l))))

(defun vp-lea-rcr (b i d) (emit (static-qqp (lea-rcr ,b ,(eval i (penv)) ,d))))
(defun vp-lea-rrr (b i d) (emit `(lea-rrr ,b ,i ,d)))
(defun vp-lea-pr (l r) (emit `(lea-pr ,(label-sym l) ,r)))
(defun vp-load-pr (l d) (emit `(load-pr ,(label-sym l) ,d)))

;d by s to d, and what is left of d by s to d: by 0 the answer is 0 and
;what is left is d, on every CPU
(defun vp-div-rr (s d) (emit `(div-rr ,s ,d)))
(defun vp-div-rr-u (s d) (emit `(div-rr-u ,s ,d)))
(defun vp-rem-rr (s d) (emit `(rem-rr ,s ,d)))
(defun vp-rem-rr-u (s d) (emit `(rem-rr-u ,s ,d)))

(defun vp-beq-cr (c d l) (emit (static-qqp (beq-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bne-cr (c d l) (emit (static-qqp (bne-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-blt-cr (c d l) (emit (static-qqp (blt-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bgt-cr (c d l) (emit (static-qqp (bgt-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-ble-cr (c d l) (emit (static-qqp (ble-cr ,(eval c (penv)) ,d ,(label-sym l)))))
(defun vp-bge-cr (c d l) (emit (static-qqp (bge-cr ,(eval c (penv)) ,d ,(label-sym l)))))

(defun vp-beq-rr (s d l) (emit (static-qqp (beq-rr ,s ,d ,(label-sym l)))))
(defun vp-bne-rr (s d l) (emit (static-qqp (bne-rr ,s ,d ,(label-sym l)))))
(defun vp-blt-rr (s d l) (emit (static-qqp (blt-rr ,s ,d ,(label-sym l)))))
(defun vp-bgt-rr (s d l) (emit (static-qqp (bgt-rr ,s ,d ,(label-sym l)))))
(defun vp-ble-rr (s d l) (emit (static-qqp (ble-rr ,s ,d ,(label-sym l)))))
(defun vp-bge-rr (s d l) (emit (static-qqp (bge-rr ,s ,d ,(label-sym l)))))

(defun vp-seq-cr (c d) (emit (static-qqp (seq-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sne-cr (c d) (emit (static-qqp (sne-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-slt-cr (c d) (emit (static-qqp (slt-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sgt-cr (c d) (emit (static-qqp (sgt-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sle-cr (c d) (emit (static-qqp (sle-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sge-cr (c d) (emit (static-qqp (sge-cr ,(logior 0 (eval c (penv))) ,d))))

(defun vp-seq-rr (s d) (emit `(seq-rr ,s ,d)))
(defun vp-sne-rr (s d) (emit `(sne-rr ,s ,d)))
(defun vp-slt-rr (s d) (emit `(slt-rr ,s ,d)))
(defun vp-sgt-rr (s d) (emit `(sgt-rr ,s ,d)))
(defun vp-sle-rr (s d) (emit `(sle-rr ,s ,d)))
(defun vp-sge-rr (s d) (emit `(sge-rr ,s ,d)))

(defun vp-cpy-cr (c d) (emit (static-qqp (cpy-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-add-cr (c d) (emit (static-qqp (add-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-sub-cr (c d) (emit (static-qqp (sub-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-mul-cr (c d) (emit (static-qqp (mul-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-and-cr (c d) (emit (static-qqp (and-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-or-cr (c d) (emit (static-qqp (or-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-xor-cr (c d) (emit (static-qqp (xor-cr ,(logior 0 (eval c (penv))) ,d))))
(defun vp-shl-cr (c d) (emit (static-qqp (shl-cr ,(eval c (penv)) ,d))))
(defun vp-shr-cr (c d) (emit (static-qqp (shr-cr ,(eval c (penv)) ,d))))
(defun vp-asr-cr (c d) (emit (static-qqp (asr-cr ,(eval c (penv)) ,d))))

(defun vp-cpy-rr (s d) (emit `(cpy-rr ,s ,d)))
(defun vp-add-rr (s d) (emit `(add-rr ,s ,d)))
(defun vp-sub-rr (s d) (emit `(sub-rr ,s ,d)))
(defun vp-mul-rr (s d) (emit `(mul-rr ,s ,d)))
(defun vp-and-rr (s d) (emit `(and-rr ,s ,d)))
(defun vp-or-rr (s d) (emit `(or-rr ,s ,d)))
(defun vp-xor-rr (s d) (emit `(xor-rr ,s ,d)))
(defun vp-shl-rr (s d) (emit `(shl-rr ,s ,d)))
(defun vp-shr-rr (s d) (emit `(shr-rr ,s ,d)))
(defun vp-asr-rr (s d) (emit `(asr-rr ,s ,d)))
(defun vp-lnot-rr (d) (emit `(lnot-rr ,d)))
(defun vp-land-rr (s d) (emit `(land-rr ,s ,d)))
(defun vp-swp-rr (s d) (emit `(swp-rr ,s ,d)))
(defun vp-ext-rr (s d) (emit `(ext-rr ,s ,d)))

(defun vp-load-rrr-b (b i d) (emit `(load-rrr-b ,b ,i ,d)))
(defun vp-load-rrr-s (b i d) (emit `(load-rrr-s ,b ,i ,d)))
(defun vp-load-rrr-i (b i d) (emit `(load-rrr-i ,b ,i ,d)))
(defun vp-load-rrr-ub (b i d) (emit `(load-rrr-ub ,b ,i ,d)))
(defun vp-load-rrr-us (b i d) (emit `(load-rrr-us ,b ,i ,d)))
(defun vp-load-rrr-ui (b i d) (emit `(load-rrr-ui ,b ,i ,d)))
(defun vp-load-rrr (b i d) (emit `(load-rrr ,b ,i ,d)))

(defun vp-store-rrr-b (s b i) (emit `(store-rrr-b ,s ,b ,i)))
(defun vp-store-rrr-s (s b i) (emit `(store-rrr-s ,s ,b ,i)))
(defun vp-store-rrr-i (s b i) (emit `(store-rrr-i ,s ,b ,i)))
(defun vp-store-rrr (s b i) (emit `(store-rrr ,s ,b ,i)))

(defun vp-load-rcr-b (b i d) (emit (static-qqp (load-rcr-b ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr-s (b i d) (emit (static-qqp (load-rcr-s ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr-i (b i d) (emit (static-qqp (load-rcr-i ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr-ub (b i d) (emit (static-qqp (load-rcr-ub ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr-us (b i d) (emit (static-qqp (load-rcr-us ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr-ui (b i d) (emit (static-qqp (load-rcr-ui ,b ,(eval i (penv)) ,d))))
(defun vp-load-rcr (b i d) (emit (static-qqp (load-rcr ,b ,(eval i (penv)) ,d))))

(defun vp-store-rrc-b (s b i) (emit (static-qqp (store-rrc-b ,s ,b ,(eval i (penv))))))
(defun vp-store-rrc-s (s b i) (emit (static-qqp (store-rrc-s ,s ,b ,(eval i (penv))))))
(defun vp-store-rrc-i (s b i) (emit (static-qqp (store-rrc-i ,s ,b ,(eval i (penv))))))
(defun vp-store-rrc (s b i) (emit (static-qqp (store-rrc ,s ,b ,(eval i (penv))))))

(defun vp-min-cr (c d) (emit (static-qqp (min-cr ,(eval c (penv)) ,d))))
(defun vp-max-cr (c d) (emit (static-qqp (max-cr ,(eval c (penv)) ,d))))
(defun vp-min-rr (s d) (emit (static-qqp (min-rr ,s ,d))))
(defun vp-max-rr (s d) (emit (static-qqp (max-rr ,s ,d))))
(defun vp-abs-rr (s d) (emit (static-qqp (abs-rr ,s ,d))))

;each op says the width it is of, -ff of singles, :fN, -dd of doubles, :dN,
;and its registers are as given, trace -i says if one is not of its op.
;A copy of a single is of the whole register
(defun vp-cpy-ff (s d) (emit `(cpy-dd ,(vp-reg-whole s) ,(vp-reg-whole d))))
(defun vp-cpy-dd (s d) (emit `(cpy-dd ,s ,d)))
(defun vp-cpy-rf (s d) (emit `(cpy-rf ,s ,d)))
(defun vp-cpy-fr (s d) (emit `(cpy-fr ,s ,d)))
(defun vp-cpy-rd (s d) (emit `(cpy-rd ,s ,d)))
(defun vp-cpy-dr (s d) (emit `(cpy-dr ,s ,d)))
(defun vp-cvt-rf (s d) (emit `(cvt-rf ,s ,d)))
(defun vp-cvt-fr (s d) (emit `(cvt-fr ,s ,d)))
(defun vp-cvt-rd (s d) (emit `(cvt-rd ,s ,d)))
(defun vp-cvt-dr (s d) (emit `(cvt-dr ,s ,d)))
;a single to a double, exactly, and a double to the nearest single
(defun vp-cvt-fd (s d) (emit `(cvt-fd ,s ,d)))
(defun vp-cvt-df (s d) (emit `(cvt-df ,s ,d)))

(defun vp-add-ff (s d) (emit `(add-ff ,s ,d)))
(defun vp-sub-ff (s d) (emit `(sub-ff ,s ,d)))
(defun vp-mul-ff (s d) (emit `(mul-ff ,s ,d)))
(defun vp-div-ff (s d) (emit `(div-ff ,s ,d)))
(defun vp-min-ff (s d) (emit `(min-ff ,s ,d)))
(defun vp-max-ff (s d) (emit `(max-ff ,s ,d)))
(defun vp-sqrt-ff (s d) (emit `(sqrt-ff ,s ,d)))
(defun vp-abs-ff (s d) (emit `(abs-ff ,s ,d)))
(defun vp-neg-ff (s d) (emit `(neg-ff ,s ,d)))

(defun vp-add-dd (s d) (emit `(add-dd ,s ,d)))
(defun vp-sub-dd (s d) (emit `(sub-dd ,s ,d)))
(defun vp-mul-dd (s d) (emit `(mul-dd ,s ,d)))
(defun vp-div-dd (s d) (emit `(div-dd ,s ,d)))
(defun vp-min-dd (s d) (emit `(min-dd ,s ,d)))
(defun vp-max-dd (s d) (emit `(max-dd ,s ,d)))
(defun vp-sqrt-dd (s d) (emit `(sqrt-dd ,s ,d)))
(defun vp-abs-dd (s d) (emit `(abs-dd ,s ,d)))
(defun vp-neg-dd (s d) (emit `(neg-dd ,s ,d)))

(defun vp-beq-ff (s d l) (emit `(beq-ff ,s ,d ,(label-sym l))))
(defun vp-bne-ff (s d l) (emit `(bne-ff ,s ,d ,(label-sym l))))
(defun vp-blt-ff (s d l) (emit `(blt-ff ,s ,d ,(label-sym l))))
(defun vp-bgt-ff (s d l) (emit `(bgt-ff ,s ,d ,(label-sym l))))
(defun vp-ble-ff (s d l) (emit `(ble-ff ,s ,d ,(label-sym l))))
(defun vp-bge-ff (s d l) (emit `(bge-ff ,s ,d ,(label-sym l))))

(defun vp-beq-dd (s d l) (emit `(beq-dd ,s ,d ,(label-sym l))))
(defun vp-bne-dd (s d l) (emit `(bne-dd ,s ,d ,(label-sym l))))
(defun vp-blt-dd (s d l) (emit `(blt-dd ,s ,d ,(label-sym l))))
(defun vp-bgt-dd (s d l) (emit `(bgt-dd ,s ,d ,(label-sym l))))
(defun vp-ble-dd (s d l) (emit `(ble-dd ,s ,d ,(label-sym l))))
(defun vp-bge-dd (s d l) (emit `(bge-dd ,s ,d ,(label-sym l))))

;a single is 4 bytes in memory, a double 8
(defun vp-load-rcf (b c d) (emit (static-qqp (load-rcf ,b ,(eval c (penv)) ,d))))
(defun vp-store-frc (s b c) (emit (static-qqp (store-frc ,s ,b ,(eval c (penv))))))
(defun vp-load-rrf (b i d) (emit `(load-rrf ,b ,i ,d)))
(defun vp-store-frr (s b i) (emit `(store-frr ,s ,b ,i)))
(defun vp-load-rcd (b c d) (emit (static-qqp (load-rcd ,b ,(eval c (penv)) ,d))))
(defun vp-store-drc (s b c) (emit (static-qqp (store-drc ,s ,b ,(eval c (penv))))))
(defun vp-load-rrd (b i d) (emit `(load-rrd ,b ,i ,d)))
(defun vp-store-drr (s b i) (emit `(store-drr ,s ,b ,i)))

;puedo vp instructions
(defun vp-simd (op &rest v)
	(defq max_len (max-length v))
	(each (# (while (< (length %0) max_len) (push %0 (last %0)))) v)
	(each! op v))
