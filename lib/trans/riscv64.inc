;;;;;;;;;;;;;;;;;;;;;;;;
; RISCV64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defcvar 'stack_align 16 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r30))
(defun emit-native-reg? (r)
	(find r '(:r28 :r30 :rsp :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
			:r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r29)))
(each (# (defcvar %0 %0)) '(:r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30))

(defmacro riscv64-r (o rd rs1 rs2 f3 f7)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12) (<< ,f7 25))))

(defmacro riscv64-i (o rd rs1 f3 imm)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,f3 12) (<< ,imm 20))))

(defmacro riscv64-s (o rs1 rs2 f3 imm)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1f ,imm) 7) (<< (logand 0xfe0 ,imm) 20))))

(defmacro riscv64-b (o rs1 rs2 f3 imm)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1e ,imm) 7) (<< (logand 0x7e0 ,imm) 20)
		(>> (logand 0x400 ,imm) 5) (<< (logand 0x800 ,imm) 18))))

(defmacro riscv64-u (o rd imm)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,imm 12))))

(defmacro riscv64-j (o rd imm)
	`(emit-int (+ ,o (<< ,rd 7)
		(<< (logand 0x7fe ,imm) 20) (<< (logand 0x400 ,imm) 9)
		(logand 0xff000 ,imm) (<< (logand 0x80000 ,imm) 11))))

(defun emit-shl-rr (s d)
	(riscv64-r 0x33 d d s 0x1 0x0))

(defun emit-shr-rr (s d)
	(riscv64-r 0x33 d d s 0x5 0x0))

(defun emit-asr-rr (s d)
	(riscv64-r 0x33 d d s 0x5 0x20))

(defun emit-shl-cr (c r)
	(riscv64-i 0x13 r r 0x1 c))

(defun emit-shr-cr (c r)
	(riscv64-i 0x13 r r 0x5 c))

(defun emit-asr-cr (c r)
	(riscv64-i 0x13 r r 0x5 (+ c 0b10000000000)))

(defun emit-cpy-rr (s d)
	(cond
		((eql s d))
		(:t (riscv64-r 0x33 d s (emit-native-reg? :r28) 0x0 0x0))))

(defun emit-add-rr (s d)
	(riscv64-r 0x33 d d s 0x0 0x0))

(defun emit-sub-rr (s d)
	(riscv64-r 0x33 d d s 0x0 0x20))

(defun emit-xor-rr (s d)
	(riscv64-r 0x33 d d s 0x4 0x0))

(defun emit-or-rr (s d)
	(riscv64-r 0x33 d d s 0x6 0x0))

(defun emit-and-rr (s d)
	(riscv64-r 0x33 d d s 0x7 0x0))

(defun emit-mul-rr (s d)
	(riscv64-r 0x33 d d s 0x0 0x1))

(defun emit-div-rrr (s d1 d2)
	(riscv64-r 0x33 (const (emit-native-reg? :r29)) d1 d2 0x4 0x1)
	(riscv64-r 0x33 d1 d1 d2 0x6 0x1)
	(emit-cpy-rr (const (emit-native-reg? :r29)) d2))

(defun emit-div-rrr-u (s d1 d2)
	(riscv64-r 0x33 (const (emit-native-reg? :r29)) d1 d2 0x5 0x1)
	(riscv64-r 0x33 d1 d1 d2 0x7 0x1)
	(emit-cpy-rr (const (emit-native-reg? :r29)) d2))

(defun riscv64-cr (c r)
	(cond
		((<= -0x800 c 0x7ff)
			(riscv64-i 0x13 r (emit-native-reg? :r28) 0x0 c))
		((<= -0x100000000 c 0xffffffff)
			(if (= (logand 0x800 c) 0)
				(defq u (>>> c 12))
				(defq u (inc (>>> c 12))))
			(riscv64-u 0x37 r u)
			(riscv64-i 0x13 r r 0x0 (logand 0xfff c)))
		(:t (throw "riscv64-cr constant out of range !" c))))

(defun emit-cpy-cr (c r)
	(cond
		((<= -0x100000000 c 0xffffffff)
			(riscv64-cr c r))
		(:t (defq l (logand c 0xffffffff))
			(if (= (logand 0x800000000 c) 0)
				(defq u (>>> c 32))
				(defq u (inc (>>> c 32))))
			(riscv64-cr u (const (emit-native-reg? :r29)))
			(riscv64-cr l r)
			(emit-shl-cr 32 (const (emit-native-reg? :r29)))
			(emit-add-rr (const (emit-native-reg? :r29)) r))))

(defun emit-add-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 c 0x7ff)
			(riscv64-i 0x13 r r 0x0 c))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-add-rr (const (emit-native-reg? :r29)) r))))

(defun emit-sub-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 (setq c (neg c)) 0x7ff)
			(riscv64-i 0x13 r r 0x0 c))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-sub-rr (const (emit-native-reg? :r29)) r))))

(defun emit-xor-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 c 0x7ff)
			(riscv64-i 0x13 r r 0x4 c))
		(:t (throw "emit-xor-cr constant out of range !" c))))

(defun emit-or-cr (c r)
	(cond
		((= c 0))
		((= c -1)
			(emit-cpy-cr -1 r))
		((<= -0x800 c 0x7ff)
			(riscv64-i 0x13 r r 0x6 c))
		(:t (throw "emit-or-cr constant out of range !" c))))

(defun emit-and-cr (c r)
	(cond
		((= c -1))
		((= c 0)
			(emit-xor-rr r r))
		((<= -0x800 c 0x7ff)
			(riscv64-i 0x13 r r 0x7 c))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-and-rr (const (emit-native-reg? :r29)) r))))

(defun emit-mul-cr (c r)
	(cond
		((= c 0)
			(emit-xor-rr r r))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-mul-rr (const (emit-native-reg? :r29)) r))))

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s (const (emit-native-reg? :r29)))
		(emit-cpy-rr d s)
		(emit-cpy-rr (const (emit-native-reg? :r29)) d)))

(defun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))

(defun riscv64-branch (o d s l)
	(cond
		((<= -0x2000 (defq c (- l *pc*)) 0x1fff)
			(riscv64-b 0x63 d s o c))
		(:t (throw "riscv64-branch constant out of range !" c))))

(defun emit-beq-rr (s d l m) (riscv64-branch 0x0 d s l))
(defun emit-bne-rr (s d l m) (riscv64-branch 0x1 d s l))
(defun emit-blt-rr (s d l m) (riscv64-branch 0x4 d s l))
(defun emit-bge-rr (s d l m) (riscv64-branch 0x5 d s l))
(defun emit-ble-rr (s d l m) (riscv64-branch 0x5 s d l))
(defun emit-bgt-rr (s d l m) (riscv64-branch 0x4 s d l))

(defun emit-beq-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x0 d (const (emit-native-reg? :r29)) l))
(defun emit-bne-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x1 d (const (emit-native-reg? :r29)) l))
(defun emit-blt-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x4 d (const (emit-native-reg? :r29)) l))
(defun emit-bge-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x5 d (const (emit-native-reg? :r29)) l))
(defun emit-ble-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x5 d (const (emit-native-reg? :r29)) l))
(defun emit-bgt-cr (c d l m)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-branch 0x4 d (const (emit-native-reg? :r29)) l))

(defun riscv64-set (o d s c)
	(riscv64-b 0x63 d s o c))

(defun emit-seq-rr (s d)
	(riscv64-set 0x0 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sne-rr (s d)
	(riscv64-set 0x1 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-slt-rr (s d)
	(riscv64-set 0x4 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sge-rr (s d)
	(riscv64-set 0x5 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sle-rr (s d)
	(riscv64-set 0x5 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sgt-rr (s d)
	(riscv64-set 0x4 d s 8)
	(emit-xor-rr d d)
	(riscv64-set 0x4 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))

(defun emit-seq-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x0 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sne-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x1 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-slt-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x4 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sge-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x5 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sle-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x5 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))
(defun emit-sgt-cr (c d)
	(emit-cpy-cr c (const (emit-native-reg? :r29)))
	(riscv64-set 0x4 d (const (emit-native-reg? :r29)) 8)
	(emit-xor-rr d d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) (const (emit-native-reg? :r28)) 4)
	(emit-or-cr -1 d))

(defun emit-lnot-rr (r d)
	(emit-seq-rr (const (emit-native-reg? :r28)) d))

(defun emit-land-rr (s d)
	(riscv64-set 0x0 (const (emit-native-reg? :r28)) d 8)
	(riscv64-set 0x1 (const (emit-native-reg? :r28)) s 4)
	(emit-xor-rr d d))

(defun riscv64-ir (o s i d)
	(cond
		((<= -0x1000 i 0xfff)
			(riscv64-i 0x3 d s o i))
		(:t (throw "riscv64-ir index out of range !" i))))

(defun emit-cpy-ir-b (s i d) (riscv64-ir 0x0 s i d))
(defun emit-cpy-ir-s (s i d) (riscv64-ir 0x1 s i d))
(defun emit-cpy-ir-i (s i d) (riscv64-ir 0x2 s i d))
(defun emit-cpy-ir (s i d) (riscv64-ir 0x3 s i d))
(defun emit-cpy-ir-ub (s i d) (riscv64-ir 0x4 s i d))
(defun emit-cpy-ir-us (s i d) (riscv64-ir 0x5 s i d))
(defun emit-cpy-ir-ui (s i d) (riscv64-ir 0x6 s i d))

(defun riscv64-ri (o s d i)
	(cond
		((<= -0x1000 i 0xfff)
			(riscv64-s 0x23 s d o i))
		(:t (throw "riscv64-ri index out of range !" i))))

(defun emit-cpy-ri-b (s d i) (riscv64-ir 0x0 s d i))
(defun emit-cpy-ri-s (s d i) (riscv64-ir 0x1 s d i))
(defun emit-cpy-ri-i (s d i) (riscv64-ir 0x2 s d i))
(defun emit-cpy-ri (s d i) (riscv64-ir 0x3 s d i))

(defun riscv64-dr (o s i d)
	(riscv64-r 0x33 (const (emit-native-reg? :r29)) s i 0x0 0x0)
	(riscv64-i 0x3 d (const (emit-native-reg? :r29)) o 0))

(defun emit-cpy-dr-b (s i d) (riscv64-dr 0x0 s i d))
(defun emit-cpy-dr-s (s i d) (riscv64-dr 0x1 s i d))
(defun emit-cpy-dr-i (s i d) (riscv64-dr 0x2 s i d))
(defun emit-cpy-dr (s i d) (riscv64-dr 0x3 s i d))
(defun emit-cpy-dr-ub (s i d) (riscv64-dr 0x4 s i d))
(defun emit-cpy-dr-us (s i d) (riscv64-dr 0x5 s i d))
(defun emit-cpy-dr-ui (s i d) (riscv64-dr 0x6 s i d))

(defun riscv64-rd (o s d i)
	(riscv64-r 0x33 (const (emit-native-reg? :r29)) d i 0x0 0x0)
	(riscv64-i 0x23 s (const (emit-native-reg? :r29)) o 0))

(defun emit-cpy-rd-b (s d i) (riscv64-rd 0x0 s d i))
(defun emit-cpy-rd-s (s d i) (riscv64-rd 0x1 s d i))
(defun emit-cpy-rd-i (s d i) (riscv64-rd 0x2 s d i))
(defun emit-cpy-rd (s d i) (riscv64-rd 0x3 s d i))

(defun emit-lea-i (s i d)
	(cond
		((and (= i 0) (eql s d)))
		((<= -0x800 i 0x7ff)
			(riscv64-i 0x13 d s 0x0 i))
		(:t (throw "emit-lea-i constant out of range !" i))))

(defun emit-lea-d (s i d) (riscv64-r 0x33 d s i 0x0 0x0))

(defun emit-push (&rest b)
	(when (/= 0 (defq l (length b)))
		(emit-sub-cr (align (* +ptr_size l) stack_align) (const (emit-native-reg? :rsp)))
		(defq _ l)
		(while (>= (setq _ (dec _)) 0)
			(emit-cpy-ri (elem-get _ b) (const (emit-native-reg? :rsp)) (* +ptr_size (- l _ 1))))))

(defun emit-pop (&rest b)
	(when (/= 0 (defq l (length b)))
		(defq _ l)
		(while (>= (setq _ (dec _)) 0)
			(emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (- l _ 1)) (elem-get _ b)))
		(emit-add-cr (align (* +ptr_size l) stack_align) (const (emit-native-reg? :rsp)))))

(defun emit-call (l)
	(emit-push (const (emit-native-reg? :r30)))
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(riscv64-j 0x6f (const (emit-native-reg? :r30)) c))
		(:t (throw "emit-call constant out of range !" c)))
	(emit-pop (const (emit-native-reg? :r30))))

(defun emit-jmp (l m)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(riscv64-j 0x6f (const (emit-native-reg? :r28)) c))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-call-r (r)
	(emit-push (const (emit-native-reg? :r30)))
	(riscv64-i 0x67 (const (emit-native-reg? :r30)) r 0x0 0x0)
	(emit-pop (const (emit-native-reg? :r30))))

(defun emit-jmp-r (r)
	(riscv64-i 0x67 (const (emit-native-reg? :r28)) r 0x0 0x0))

(defun emit-call-i (s i)
	(emit-cpy-ir s i (const (emit-native-reg? :r29)))
	(emit-call-r (const (emit-native-reg? :r29))))

(defun emit-jmp-i (s i)
	(emit-cpy-ir s i (const (emit-native-reg? :r29)))
	(emit-jmp-r (const (emit-native-reg? :r29))))

(defun emit-lea-p (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(riscv64-u 0x17 d c))
		(:t (throw "emit-lea-p constant out of range !" c))))

(defun emit-cpy-pr (l d)
	(emit-lea-p l (const (emit-native-reg? :r27)))
	(emit-cpy-ir (const (emit-native-reg? :r27)) 0 d))

(defun emit-call-p (l)
	(emit-cpy-pr l (const (emit-native-reg? :r29)))
	(emit-call-r (const (emit-native-reg? :r29))))

(defun emit-jmp-p (l)
	(emit-cpy-pr l (const (emit-native-reg? :r29)))
	(emit-jmp-r (const (emit-native-reg? :r29))))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-free (c) (emit-add-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-ret () (emit-jmp-r (const (emit-native-reg? :r30))))
(defun emit-sync (n) (emit-int 0x0ff0000f))
(defun emit-brk (n))

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-and-cr (neg stack_align) s)
	(emit-sub-cr tk_state_size s)
	(emit-cpy-ri f s (- tk_state_size +ptr_size))
	(emit-cpy-ri x s (- tk_state_size (* +ptr_size 2)))
	(emit-lea-p (defcvar (label-sym 'riscv_start) (+ *pc* 12)) f)
	(emit-cpy-ri f s 0)
	(emit-jmp (defcvar (label-sym 'riscv_exit) (+ *pc* 12)) 0)
;riscv_start
	(emit-cpy-rr (const (emit-native-reg? :r1)) (const (emit-native-reg? :r30)))
	(emit-jmp-r (const (emit-native-reg? :r0))))
;riscv_exit

(case *abi*
(RISCV64
(defun emit-call-abi (r b c n &rest x)
	(setq x (cat `(,(emit-native-reg? :r30)) x))
	(apply (const emit-push) x)
	(defq l (length x))
	(emit-cpy-ir b c (const (emit-native-reg? :r29)))
	(riscv64-i 0x67 (const (emit-native-reg? :r30)) (const (emit-native-reg? :r29)) 0x0 0x0)
	(emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (dec l)) (const (emit-native-reg? :r30)))
	(emit-free (* +ptr_size l))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))
