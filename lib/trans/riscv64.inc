;;;;;;;;;;;;;;;;;;;;;;;;
; RISCV64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defcvar 'stack_align 8 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
(defun emit-native-reg? (r) (find r +vp_regs))

(defq opcode 0)
(defmacro next-opcode (&optional i)
	(defq ci opcode)
	(setq opcode (+ opcode (opt i 1)))
	ci)

(defun emit-cr (o c r)
	(cond
		((<= -0x8 c 0x7)
			(emit-short (+ o (<< r 8) (<< c 12))))
		((<= -0x80000 c 0x7ffff)
			(emit-short (+ (inc o) (<< r 8) (<< c 12)) (>>> c 4)))
		((<= -0x800000000 c 0x7ffffffff)
			(emit-short (+ (+ o 2) (<< r 8) (<< c 12)) (>>> c 4) (>>> c 20)))
		(:t  (emit-short (+ (+ o 3) (<< r 8)) c (>>> c 16) (>>> c 32) (>>> c 48)))))

(defun emit-cpy-cr (c r) (emit-cr (next-opcode 4) c r))
(defun emit-add-cr (c r) (unless (= c 0) (emit-cr (next-opcode 4) c r)))
(defun emit-sub-cr (c r) (unless (= c 0) (emit-cr (next-opcode 4) c r)))
(defun emit-cmp-cr (c r) (emit-cr (next-opcode 4) c r))
(defun emit-mul-cr (c r) (unless (= c 1) (emit-cr (next-opcode 4) c r)))
(defun emit-and-cr (c r) (unless (= c -1) (emit-cr (next-opcode 4) c r)))
(defun emit-or-cr (c r) (unless (= c 0) (emit-cr (next-opcode 4) c r)))
(defun emit-xor-cr (c r) (unless (= c 0) (emit-cr (next-opcode 4) c r)))

(defun emit-shift-cr (o c r)
	(cond
		((= c 0))
		((<= 1 c 63)
			(emit-short (+ o (<< r 8)) c))
		(:t  (throw "emit-shift-cr constant out of range !" c))))

(defun emit-shl-cr (c r) (emit-shift-cr (next-opcode) c r))
(defun emit-shr-cr (c r) (emit-shift-cr (next-opcode) c r))
(defun emit-asr-cr (c r) (emit-shift-cr (next-opcode) c r))

(defun emit-cpy-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-add-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sub-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-cmp-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
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

(defun emit-scr (o c d)
	(cond
		((<= -0x8 c 0x7)
			(emit-short (+ o (<< d 8) (<< c 12))))
		((<= -0x80000 c 0x7ffff)
			(emit-short (+ (inc o) (<< d 8) (<< c 12)) (>>> c 4)))
		((<= -0x800000000 c 0x7ffffffff)
			(emit-short (+ (+ o 2) (<< d 8) (<< c 12)) (>>> c 4) (>>> c 20)))
		(:t  (throw "emit-scr constant out of range !" c))))

(defun emit-seq-cr (c d) (emit-scr (next-opcode 3) c d))
(defun emit-sne-cr (c d) (emit-scr (next-opcode 3) c d))
(defun emit-slt-cr (c d) (emit-scr (next-opcode 3) c d))
(defun emit-sle-cr (c d) (emit-scr (next-opcode 3) c d))
(defun emit-sgt-cr (c d) (emit-scr (next-opcode 3) c d))
(defun emit-sge-cr (c d) (emit-scr (next-opcode 3) c d))

(defun emit-seq-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sne-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-slt-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sle-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sgt-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))
(defun emit-sge-rr (s d) (emit-short (+ (next-opcode) (<< d 8) (<< s 12))))

(defun emit-branch (o l d)
	(defq m (elem-get d *branches*) l (- l *pc*))
	(and (/= *pass* 0) (> (abs l) (abs m)) (elem-set d *branches* (setq m l)))
	(cond
		((<= -0x80 (- m 2) 0x7f)
			(emit-short (+ o (<< (- l 2) 8))))
		((<= -0x800000 (- m 4) 0x7fffff)
			(emit-short (+ (inc o) (<< (- l 4) 8)) (>>> (- l 4) 8)))
		(:t  (throw "emit-branch constant out of range !" l))))

(defun emit-beq (l d) (emit-branch (next-opcode 2) l d))
(defun emit-bne (l d) (emit-branch (next-opcode 2) l d))
(defun emit-bge (l d) (emit-branch (next-opcode 2) l d))
(defun emit-blt (l d) (emit-branch (next-opcode 2) l d))
(defun emit-ble (l d) (emit-branch (next-opcode 2) l d))
(defun emit-bgt (l d) (emit-branch (next-opcode 2) l d))

(defun emit-ir (o s c d)
	(cond
		((<= -0x8000 c 0x7fff)
			(emit-short (+ o (<< d 8) (<< s 12)) c))
		(:t  (throw "emit-ir constant out of range !" c))))

(defun emit-cpy-ir (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-b (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-s (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-i (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-ub (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-us (s c d) (emit-ir (next-opcode) s c d))
(defun emit-cpy-ir-ui (s c d) (emit-ir (next-opcode) s c d))
(defun emit-lea-i (s c d) (emit-ir (next-opcode) s c d))

(defun emit-ri (o s d c)
	(cond
		((<= -0x8000 c 0x7fff)
			(emit-short (+ o (<< d 8) (<< s 12)) c))
		(:t  (throw "emit-ri constant out of range !" c))))

(defun emit-cpy-ri (s d c) (emit-ri (next-opcode) s d c))
(defun emit-cpy-ri-b (s d c) (emit-ri (next-opcode) s d c))
(defun emit-cpy-ri-s (s d c) (emit-ri (next-opcode) s d c))
(defun emit-cpy-ri-i (s d c) (emit-ri (next-opcode) s d c))

(defun emit-rd (o s d1 d2)
	(emit-short (+ o (<< d1 8) (<< d2 12)) s))

(defun emit-cpy-rd (s d1 d2) (emit-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-b (s d2 d1) (emit-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-s (s d2 d1) (emit-rd (next-opcode) s d1 d2))
(defun emit-cpy-rd-i (s d2 d1) (emit-rd (next-opcode) s d1 d2))

(defun emit-dr (o s1 s2 d)
	(emit-short (+ o (<< d 8) (<< s1 12)) s2))

(defun emit-cpy-dr (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-b (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-s (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-i (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-ub (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-us (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-cpy-dr-ui (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))
(defun emit-lea-d (s1 s2 d) (emit-dr (next-opcode) s1 s2 d))

(defun emit-call-r (r) (emit-short (+ (next-opcode) (<< r 8))))
(defun emit-jmp-r (r) (emit-short (+ (next-opcode) (<< r 8))))

(defun emit-call-jmp-i (o r c)
	(cond
		((<= -0x80000 c 0x7ffff)
			(emit-short (+ o (<< r 8) (<< c 12)) (>>> c 4)))
		(:t  (throw "emit-call-jmp-i constant out of range !" c))))

(defun emit-call-i (r c) (emit-call-jmp-i (next-opcode) r c))
(defun emit-jmp-i (r c) (emit-call-jmp-i (next-opcode) r c))

(defun emit-p (o r l)
	(cond
		((<= -0x80000 (defq c (- l *pc* 4)) 0x7ffff)
			(emit-short (+ o (<< r 8) (<< c 12)) (>>> c 4)))
		(:t  (throw "emit-p constant out of range !" c))))

(defun emit-cpy-pr (l r) (emit-p (next-opcode) r l))
(defun emit-lea-p (l r) (emit-p (next-opcode) r l))

(defun emit-call-jmp-p (o l)
	(cond
		((<= -0x80 (defq c (- l *pc* 2)) 0x7f)
			(emit-short (+ o (<< c 8))))
		((<= -0x800000 (defq c (- l *pc* 4)) 0x7fffff)
			(emit-short (+ (inc o) (<< c 8)) (>>> c 8)))
		(:t  (throw "emit-call-jmp-p constant out of range !" c))))

(defun emit-call (l) (emit-call-jmp-p (next-opcode 2) l))
(defun emit-jmp (l d) (emit-call-jmp-p (next-opcode 2) l))
(defun emit-call-p (l) (emit-call-jmp-p (next-opcode 2) l))
(defun emit-jmp-p (l) (emit-call-jmp-p (next-opcode 2) l))

(defun emit-call-abi (r b c n &rest x)
	(cond
		((<= 0 c (const (- 0x10000 +ptr_size)))
			(emit-short (+ (next-opcode) (<< b 8) (<< n 12)) c))
		(:t  (throw "emit-call-abi constant out of range !" c))))

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
