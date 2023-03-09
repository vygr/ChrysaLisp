;;;;;;;;;;;;;;
; VP Registers
;;;;;;;;;;;;;;

(defq +vp_regs ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :rsp)
	+vp_regs_0_14 ''(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
(defun vp-reg? (_) (find _ +vp_regs))
(each (# (defcvar %0 %0)) +vp_regs)

(defmacro vp-def (_ &optional l)
	(if (> (length (defq v (filter (lambda (_)
			(and (def? _ *compile_env*) (not (find _ *func_syms*)))) _))) 0)
		(throw "VP redefine !" v))
	(setq l (apply cat (map (lambda (x y)
		(list (list 'quote x) y)) _ (if l (merge-obj (cat (eval l)) +vp_regs_0_14) +vp_regs_0_14))))
	`(progn (defcvar ~l) (apply push (cat (list *func_syms*) ',_))))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defmacro emit (&rest _)
	`(push *emit_list* ~_))

(defun emit-print (_)
	(unless (or (eql (elem-get 0 _) 'lambda)
				(eql _ 'progn))
		(print _)))

(defun emit-vp-code (emit_list)
	;remove redundant labels and rename
	(defq out (push (clear '()) 'progn) labels (env 11)
		last_e :nil pc -1 pc_sym :nil)
	(each (lambda (op)
		(unless (eql (defq e (elem-get 0 op)) 'lambda)
			(cond
				((eql e 'emit-label)
					(unless (eql e last_e)
						(setq pc_sym (sym (str "_" (setq pc (inc pc)))))
						(push out op))
					(def labels (elem-get -2 (elem-get -2 op)) pc_sym))
				((push out op))))
		(setq last_e e)) emit_list)
	(walk-list out
		(lambda (e)
			(and (sym? e) (defq e (def? e labels))
				(elem-set _ _l e)))
		(lambda (l) :nil)
		(lambda (l) :t))
	(when *debug_emit* (each emit-print out))
	out)

;;;;;;;;;;;;;;
; VP optimizer
;;;;;;;;;;;;;;

(include "lib/asm/vpopt.inc")

;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(defun label-sym (s) (sym (cat "_l_" s)))
(defun vp-label (l) (setq l (label-sym l)) (emit (list 'emit-label (list 'quote l))))
(defun vp-align (a &optional b) (emit (list 'emit-align a b)))
(defun vp-string (s) (emit (list 'emit-string s)))
(defun vp-byte (&rest b) (emit (cat '(emit-byte) b)))
(defun vp-short (&rest b) (emit (cat '(emit-short) b)))
(defun vp-int (&rest b) (emit (cat '(emit-int) b)))
(defun vp-long (&rest b) (emit (cat '(emit-long) b)))
(defun vp-cstr (s) (vp-string s)(vp-byte 0))

(defmacro vp-bcr (_)
	`(defun ,(sym (cat "vp-" _ "-cr")) (c d l)
		(emit (list ',(sym (cat "emit-" _ "-cr")) (eval c (penv)) (get d) (label-sym l)))))
(defmacro vp-brr (_)
	`(defun ,(sym (cat "vp-" _ "-rr")) (s d l)
		(emit (list ',(sym (cat "emit-" _ "-rr")) s d (label-sym l)))))
(defmacro vp-op-cr (_)
	`(defun ,(sym (cat "vp-" _ "-cr")) (c d)
		(emit (list ',(sym (cat "emit-" _ "-cr")) (logior 0 (eval c (penv))) (get d)))))
(defmacro vp-op-rr (_)
	`(defun ,(sym (cat "vp-" _ "-rr")) (s d)
		(emit (list ',(sym (cat "emit-" _ "-rr")) s d))))
(defmacro vp-mem-dr (&optional _)
	(setq _ (if _ (cat "-" _) ""))
	`(defun ,(sym (cat "vp-cpy-dr" _)) (b i d)
		(emit (list ',(sym (cat "emit-cpy-dr" _)) b i d))))
(defmacro vp-mem-rd (&optional _)
	(setq _ (if _ (cat "-" _) ""))
	`(defun ,(sym (cat "vp-cpy-rd" _)) (s b i)
		(emit (list ',(sym (cat "emit-cpy-rd" _)) s b i))))
(defmacro vp-mem-ir (&optional _)
	(setq _ (if _ (cat "-" _) ""))
	`(defun ,(sym (cat "vp-cpy-ir" _)) (b i d)
		(emit (list ',(sym (cat "emit-cpy-ir" _)) b (eval i (penv)) (get d)))))
(defmacro vp-mem-ri (&optional _)
	(setq _ (if _ (cat "-" _) ""))
	`(defun ,(sym (cat "vp-cpy-ri" _)) (s b i)
		(emit (list ',(sym (cat "emit-cpy-ri" _)) s b (eval i (penv))))))

(defun vp-push (&rest b) (emit (cat '(emit-push) b)))
(defun vp-pop (&rest b) (emit (cat '(emit-pop) b)))
(defun vp-alloc (c) (emit (list 'emit-alloc (eval c (penv)))))
(defun vp-free (c) (emit (list 'emit-free (eval c (penv)))))
(defun vp-ret () (emit (list 'emit-ret)))
(defun vp-brk (&optional n) (emit (list 'emit-brk (opt n 0))))
(defun vp-sync (&optional n) (emit (list 'emit-sync (opt n 0))))
(defun vp-stack-init (s f x) (emit (list 'emit-stack-init s f x)))

(defun vp-call-abi (r b i n x) (emit (cat (list 'emit-call-abi r b i n) x)))
(defun vp-call (l) (emit (list 'emit-call (label-sym l))))
(defun vp-call-r (d) (emit (list 'emit-call-r d)))
(defun vp-call-i (b i) (emit (list 'emit-call-i b (eval i (penv)))))
(defun vp-call-p (l) (emit (list 'emit-call-p (label-sym l))))

(defun vp-jmp (l) (emit (list 'emit-jmp (label-sym l))))
(defun vp-jmp-r (d) (emit (list 'emit-jmp-r d)))
(defun vp-jmp-i (b i) (emit (list 'emit-jmp-i b (eval i (penv)))))
(defun vp-jmp-p (l) (emit (list 'emit-jmp-p (label-sym l))))

(defun vp-lea-i (b i d) (emit (list 'emit-lea-i b (eval i (penv)) d)))
(defun vp-lea-d (b i d) (emit (list 'emit-lea-d b i d)))
(defun vp-lea-p (l r) (emit (list 'emit-lea-p (label-sym l) r)))
(defun vp-cpy-pr (l d) (emit (list 'emit-cpy-pr (label-sym l) d)))

(defun vp-div-rrr (d r q) (emit (list 'emit-div-rrr d r q)))
(defun vp-div-rrr-u (d r q) (emit (list 'emit-div-rrr-u d r q)))

(vp-bcr beq) (vp-bcr bne) (vp-bcr blt) (vp-bcr bgt) (vp-bcr ble) (vp-bcr bge)
(vp-brr beq) (vp-brr bne) (vp-brr blt) (vp-brr bgt) (vp-brr ble) (vp-brr bge)
(vp-op-cr seq) (vp-op-cr sne) (vp-op-cr slt) (vp-op-cr sgt) (vp-op-cr sle) (vp-op-cr sge)
(vp-op-rr seq) (vp-op-rr sne) (vp-op-rr slt) (vp-op-rr sgt) (vp-op-rr sle) (vp-op-rr sge)

(vp-mem-dr b) (vp-mem-dr i) (vp-mem-dr s) (vp-mem-dr ub) (vp-mem-dr ui)
(vp-mem-dr us) (vp-mem-dr) (vp-mem-ir b) (vp-mem-ir i) (vp-mem-ir s)
(vp-mem-ir ub) (vp-mem-ir ui) (vp-mem-ir us) (vp-mem-ir)

(vp-mem-rd b) (vp-mem-rd i) (vp-mem-rd s) (vp-mem-rd) (vp-mem-ri b)
(vp-mem-ri i) (vp-mem-ri s) (vp-mem-ri)

(vp-op-cr cpy) (vp-op-cr add) (vp-op-cr sub) (vp-op-cr mul) (vp-op-cr and)
(vp-op-cr or) (vp-op-cr xor) (vp-op-cr shl) (vp-op-cr shr) (vp-op-cr asr)
(vp-op-rr cpy) (vp-op-rr add) (vp-op-rr sub) (vp-op-rr mul) (vp-op-rr and)
(vp-op-rr or) (vp-op-rr xor) (vp-op-rr shl) (vp-op-rr shr) (vp-op-rr asr)
(vp-op-rr lnot) (vp-op-rr land) (vp-op-rr swp) (vp-op-rr ext)

;puedo vp instructions
(defun vp-min (a b &optional c) (vp-cpy-rr b (setd c b)) (vpif (list a '< b)) (vp-cpy-rr a c) (endif))
(defun vp-max (a b &optional c) (vp-cpy-rr b (setd c b)) (vpif (list a '> b)) (vp-cpy-rr a c) (endif))
(defun vp-abs (a &optional b) (vp-cpy-rr a (setd b a)) (vpif (list a '< 0)) (vp-mul-cr -1 b) (endif))
(defun vp-vec (op &rest v)
	(unless (every (# (= (length %0) (length (elem-get 0 v)))) v)
		(throw "Mismatched vectors !" v))
	(each! 0 -1 op v))
