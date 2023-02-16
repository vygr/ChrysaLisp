;;;;;;;;;;;;;;;;;;;;;
; VP64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(defcvar 'stack_align 8 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))

(defun emit-native-reg? (r) (find r +vp_regs))

(defq opcode 0)
(defmacro next-opcode (&optional i)
	(defq ci opcode)
	(setq opcode (+ opcode (opt i 1)))
	ci)

(defmacro vp64-within (c n)
	`(<= ,(neg (<< 1 (- n 1))) ,c ,(>> -1 (- 65 n))))

(defun vp64-cr (o c r)
	(cond
		((vp64-within c 4)
			(emit-short (+ o (<< r 8) (<< c 12))))
		((vp64-within c 20)
			(emit-short (+ (inc o) (<< r 8) (<< c 12)) (>>> c 4)))
		((vp64-within c 36)
			(emit-short (+ (+ o 2) (<< r 8) (<< c 12)) (>>> c 4) (>>> c 20)))
		(:t  (emit-short (+ (+ o 3) (<< r 8)) c (>>> c 16) (>>> c 32) (>>> c 48)))))

(defun emit-cpy-cr (c r) (vp64-cr (next-opcode 4) c r))
(defun emit-add-cr (c r) (unless (= c 0) (vp64-cr (next-opcode 4) c r)))
(defun emit-sub-cr (c r) (unless (= c 0) (vp64-cr (next-opcode 4) c r)))
(defun vp64-cmp-cr (c r) (vp64-cr (next-opcode 4) c r))
(defun emit-mul-cr (c r) (unless (= c 1) (vp64-cr (next-opcode 4) c r)))
(defun emit-and-cr (c r) (unless (= c -1) (vp64-cr (next-opcode 4) c r)))
(defun emit-or-cr (c r) (unless (= c 0) (vp64-cr (next-opcode 4) c r)))
(defun emit-xor-cr (c r) (unless (= c 0) (vp64-cr (next-opcode 4) c r)))

(defun vp64-shift-cr (o c r)
	(cond
		((= c 0))
		((<= 1 c 63)
			(emit-short (+ o (<< r 8)) c))
		(:t  (throw "vp64-shift-cr constant out of range !" c))))

(defun emit-shl-cr (c r) (vp64-shift-cr (next-opcode) c r))
(defun emit-shr-cr (c r) (vp64-shift-cr (next-opcode) c r))
(defun emit-asr-cr (c r) (vp64-shift-cr (next-opcode) c r))

(defun emit-cpy-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-add-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sub-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun vp64-cmp-r (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-mul-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-and-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-or-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-xor-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-shl-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-shr-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-asr-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-lnot-rr (r d) (emit-short (+ (next-opcode) (<< d 8) (<< r 12))))
(defun emit-land-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-swp-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-ext-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-div-rrr (s d2 d1) (emit-short (+ (next-opcode) (<< d1 8) (<< d2 12)) s))
(defun emit-div-rrr-u (s d2 d1) (emit-short (+ (next-opcode) (<< d1 8) (<< d2 12)) s))

(defun vp64-scr (o c d)
	(cond
		((vp64-within c 4)
			(emit-short (+ o (<< d 8) (<< c 12))))
		((vp64-within c 20)
			(emit-short (+ (inc o) (<< d 8) (<< c 12)) (>>> c 4)))
		((vp64-within c 36)
			(emit-short (+ (+ o 2) (<< d 8) (<< c 12)) (>>> c 4) (>>> c 20)))
		(:t  (throw "vp64-scr constant out of range !" c))))

(defun emit-seq-cr (c d) (vp64-scr (next-opcode 3) c d))
(defun emit-sne-cr (c d) (vp64-scr (next-opcode 3) c d))
(defun emit-slt-cr (c d) (vp64-scr (next-opcode 3) c d))
(defun emit-sle-cr (c d) (vp64-scr (next-opcode 3) c d))
(defun emit-sgt-cr (c d) (vp64-scr (next-opcode 3) c d))
(defun emit-sge-cr (c d) (vp64-scr (next-opcode 3) c d))

(defun emit-seq-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sne-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-slt-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sle-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sgt-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sge-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))

(defun vp64-branch (o l d)
	(defq m (elem-get d *branches*) l (- l *pc*))
	(and (> *pass* 0) (> (abs l) (abs m)) (elem-set d *branches* (setq m l)))
	(cond
		((vp64-within (- m 2) 8)
			(emit-short (+ o (<< (- l 2) 8))))
		((vp64-within (- m 4) 24)
			(emit-short (+ (inc o) (<< (- l 4) 8)) (>>> (- l 4) 8)))
		(:t  (throw "vp64-branch constant out of range !" l))))

(defun vp64-beq (l d) (vp64-branch (next-opcode 2) l d))
(defun vp64-bne (l d) (vp64-branch (next-opcode 2) l d))
(defun vp64-bge (l d) (vp64-branch (next-opcode 2) l d))
(defun vp64-blt (l d) (vp64-branch (next-opcode 2) l d))
(defun vp64-ble (l d) (vp64-branch (next-opcode 2) l d))
(defun vp64-bgt (l d) (vp64-branch (next-opcode 2) l d))

(defun emit-beq-cr (c d l m) (vp64-cmp-cr c d) (vp64-beq l m))
(defun emit-bne-cr (c d l m) (vp64-cmp-cr c d) (vp64-bne l m))
(defun emit-bge-cr (c d l m) (vp64-cmp-cr c d) (vp64-bge l m))
(defun emit-blt-cr (c d l m) (vp64-cmp-cr c d) (vp64-blt l m))
(defun emit-ble-cr (c d l m) (vp64-cmp-cr c d) (vp64-ble l m))
(defun emit-bgt-cr (c d l m) (vp64-cmp-cr c d) (vp64-bgt l m))

(defun emit-beq-rr (s d l m) (vp64-cmp-r s d) (vp64-beq l m))
(defun emit-bne-rr (s d l m) (vp64-cmp-r s d) (vp64-bne l m))
(defun emit-bge-rr (s d l m) (vp64-cmp-r s d) (vp64-bge l m))
(defun emit-blt-rr (s d l m) (vp64-cmp-r s d) (vp64-blt l m))
(defun emit-ble-rr (s d l m) (vp64-cmp-r s d) (vp64-ble l m))
(defun emit-bgt-rr (s d l m) (vp64-cmp-r s d) (vp64-bgt l m))

(defun vp64-ir (o s c d)
	(cond
		((vp64-within c 16)
			(emit-short (+ o (<< d 8) (<< s 12)) c))
		(:t  (throw "vp64-ir constant out of range !" c))))

(defun emit-cpy-ir (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-b (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-s (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-i (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-ub (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-us (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-cpy-ir-ui (s c d) (vp64-ir (next-opcode) s c d))
(defun emit-lea-i (s c d) (vp64-ir (next-opcode) s c d))

(defun vp64-ri (o s d c)
	(cond
		((vp64-within c 16)
			(emit-short (+ o (<< d 8) (<< s 12)) c))
		(:t  (throw "vp64-ri constant out of range !" c))))

(defun emit-cpy-ri (s d c) (vp64-ri (next-opcode) s d c))
(defun emit-cpy-ri-b (s d c) (vp64-ri (next-opcode) s d c))
(defun emit-cpy-ri-s (s d c) (vp64-ri (next-opcode) s d c))
(defun emit-cpy-ri-i (s d c) (vp64-ri (next-opcode) s d c))

(defun vp64-rd (o s d1 d2)
	(emit-short (+ o (<< d1 8) (<< d2 12)) s))

(defun emit-cpy-rd (s d1 d2) (vp64-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-b (s d2 d1) (vp64-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-s (s d2 d1) (vp64-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-i (s d2 d1) (vp64-rd (next-opcode) s d1 d2))

(defun vp64-dr (o s1 s2 d)
	(emit-short (+ o (<< d 8) (<< s1 12)) s2))

(defun emit-cpy-dr (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-b (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-s (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-i (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-ub (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-us (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-ui (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))
(defun emit-lea-d (s1 s2 d) (vp64-dr (next-opcode) s1 s2 d))

(defun emit-call-r (r) (emit-short (+ (next-opcode) (<< r 8))))
(defun emit-jmp-r (r) (emit-short (+ (next-opcode) (<< r 8))))

(defun vp64-call-jmp-i (o r c)
	(cond
		((vp64-within c 20)
			(emit-short (+ o (<< r 8) (<< c 12)) (>>> c 4)))
		(:t  (throw "vp64-call-jmp-i constant out of range !" c))))

(defun emit-call-i (r c) (vp64-call-jmp-i (next-opcode) r c))
(defun emit-jmp-i (r c) (vp64-call-jmp-i (next-opcode) r c))

(defun vp64-p (o r l)
	(cond
		((vp64-within (defq c (- l *pc* 4)) 20)
			(emit-short (+ o (<< r 8) (<< c 12)) (>>> c 4)))
		(:t  (throw "vp64-p constant out of range !" c))))

(defun emit-cpy-pr (l r) (vp64-p (next-opcode) r l))
(defun emit-lea-p (l r) (vp64-p (next-opcode) r l))

(defun vp64-call-jmp-p (o l)
	(cond
		((vp64-within (defq c (- l *pc* 2)) 8)
			(emit-short (+ o (<< c 8))))
		((vp64-within (defq c (- l *pc* 4)) 24)
			(emit-short (+ (inc o) (<< c 8)) (>>> c 8)))
		(:t  (throw "vp64-call-jmp-p constant out of range !" c))))

(defun emit-call (l) (vp64-call-jmp-p (next-opcode 2) l))
(defun emit-jmp (l d) (vp64-call-jmp-p (next-opcode 2) l))
(defun emit-call-p (l) (vp64-call-jmp-p (next-opcode 2) l))
(defun emit-jmp-p (l) (vp64-call-jmp-p (next-opcode 2) l))

(case *abi*
(VP64
(defun emit-call-abi (r b c n &rest x)
	(cond
		((<= 0 c (const (- 0x10000 +ptr_size)))
			(emit-short (+ (next-opcode) (<< b 8) (<< n 12)) c))
		(:t  (throw "emit-call-abi constant out of range !" c)))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-free (c) (emit-add-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-ret () (emit-short (next-opcode)))
(defun emit-sync (n) (emit-short (+ (next-opcode) (<< n 8))))
(defun emit-brk (n) (emit-short (+ (next-opcode) (<< n 8))))

(defun emit-push (&rest b)
	(when (/= 0 (length b))
		(emit-alloc (* +ptr_size (length b)))
		(each-rev (lambda (r)
			(emit-cpy-ri r (const (emit-native-reg? :rsp)) (* (- (length b) 1 _) +ptr_size))) b)))

(defun emit-pop (&rest b)
	(when (/= 0 (length b))
		(each-rev (lambda (r)
			(emit-cpy-ir (const (emit-native-reg? :rsp)) (* (- (length b) 1 _) +ptr_size) r)) b))
		(emit-free (* +ptr_size (length b))))

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-sub-cr (+ tk_state_size (* +ptr_size 2)) s)
	(emit-cpy-ri f s tk_state_size)
	(emit-cpy-ri x s (+ tk_state_size +ptr_size)))
