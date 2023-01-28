;;;;;;;;;;;;;;;;;;;;;;;;
; RISCV64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defcvar 'stack_align 16 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r30))
(defun emit-native-reg? (r)
	(find r '(:r28 :r30 :rsp :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
			:r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r29)))
(each (# (defcvar %0 %0)) '(:r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30))

(defmacro emit-r (o rd rs1 rs2 f3 f7)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12) (<< ,f7 25))))

(defmacro emit-i (o rd rs1 f3 imm)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,f3 12) (<< ,imm 20))))

(defmacro emit-s (o rs1 rs2 f3 imm)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1f ,imm) 7) (<< (logand 0xfe0 ,imm) 20))))

(defmacro emit-b (o rs1 rs2 f3 imm)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1e ,imm) 7) (<< (logand 0x7e0 ,imm) 20)
		(>> (logand 0x400 ,imm) 5) (<< (logand 0x800 ,imm) 18))))

(defmacro emit-u (o rd imm)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,imm 12))))

(defmacro emit-j (o rd imm)
	`(emit-int (+ ,o (<< ,rd 7)
		(<< (logand 0x7fe ,imm) 20) (<< (logand 0x400 ,imm) 9)
		(logand 0xff000 ,imm) (<< (logand 0x80000 ,imm) 11))))

(defun emit-cpy-rr (s d)
	(cond
		((eql s d))
		(:t (emit-r 0x33 d s (emit-native-reg? :r28) 0x0 0x0))))

(defun emit-add-rr (s d)
	(emit-r 0x33 d d s 0x0 0x0))

(defun emit-sub-rr (s d)
	(emit-r 0x33 d d s 0x0 0x20))

(defun emit-xor-rr (s d)
	(emit-r 0x33 d d s 0x4 0x0))

(defun emit-or-rr (s d)
	(emit-r 0x33 d d s 0x6 0x0))

(defun emit-and-rr (s d)
	(emit-r 0x33 d d s 0x7 0x0))

(defun emit-mul-rr (s d)
	(emit-r 0x33 d d s 0x0 0x1))

(defun emit-cpy-cr (c r)
	(cond
		((<= -0x800 c 0x7ff)
			(emit-i 0x13 r (emit-native-reg? :r28) 0x0 c))
		(:t (throw "emit-cpy-cr constant out of range !" ,c))))

(defun emit-add-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 c 0x7ff)
			(emit-i 0x13 r r 0x0 c))
		(:t (throw "emit-add-cr constant out of range !" ,c))))

(defun emit-sub-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 (setq c (neg c)) 0x7ff)
			(emit-i 0x13 r r 0x0 c))
		(:t (throw "emit-sub-cr constant out of range !" ,c))))

(defun emit-xor-cr (c r)
	(cond
		((= c 0))
		((<= -0x800 c 0x7ff)
			(emit-i 0x13 r r 0x4 c))
		(:t (throw "emit-xor-cr constant out of range !" ,c))))

(defun emit-or-cr (c r)
	(cond
		((= c 0))
		((= c -1)
			(emit-cpy-cr -1 r))
		((<= -0x800 c 0x7ff)
			(emit-i 0x13 r r 0x6 c))
		(:t (throw "emit-or-cr constant out of range !" ,c))))

(defun emit-and-cr (c r)
	(cond
		((= c -1))
		((= c 0)
			(emit-xor-rr r r))
		((<= -0x800 c 0x7ff)
			(emit-i 0x13 r r 0x7 c))
		(:t (throw "emit-and-cr constant out of range !" ,c))))

(defun emit-mul-cr (c r)
	(cond
		((= c 0)
			(emit-xor-rr r r))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-mul-rr (const (emit-native-reg? :r29)) r))))

(defun emit-shl-rr (s d)
	(emit-r 0x33 d d s 0x1 0x0))

(defun emit-shr-rr (s d)
	(emit-r 0x33 d d s 0x5 0x0))

(defun emit-asr-rr (s d)
	(emit-r 0x33 d d s 0x5 0x20))

(defun emit-shl-cr (c r)
	(emit-i 0x13 r r 0x1 c))

(defun emit-shr-cr (c r)
	(emit-i 0x13 r r 0x5 c))

(defun emit-asr-cr (c r)
	(emit-i 0x13 r r 0x5 (+ c 0b10000000000)))

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s (const (emit-native-reg? :r29)))
		(emit-cpy-rr d s)
		(emit-cpy-rr (const (emit-native-reg? :r29)) d)))

(defun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))








(defun emit-cmp-rr (s d)
	(cond
		((eql s (const (emit-native-reg? :rsp)))
			(throw "emit-cmp-rr src can't be :rsp !" (list s d)))
		(:t (emit-rrr 0xeb206000 s d (const (emit-native-reg? :rsp))))))

(defun emit-cmp-cr (c r)
	(cond
		((<= 0 c 0xfff)
			(emit-int (+ 0xf100001f (<< c 10) (<< r 5))))
		((<= -0xfff c 0)
			(emit-int (+ 0xb100001f (<< (neg c) 10) (<< r 5))))
		(:t (emit-cpy-cr c (const (emit-native-reg? :r29)))
			(emit-cmp-rr (const (emit-native-reg? :r29)) r))))

(defun emit-cpy-ir (s c d)
	(if (eql d (const (emit-native-reg? :rsp)))
		(progn
			(emit-ir 3 0xf8400000 s c (const (emit-native-reg? :r29)))
			(emit-cpy-rr (const (emit-native-reg? :r29)) (const (emit-native-reg? :rsp))))
		(emit-ir 3 0xf8400000 s c d)))

(defmacro emit-iru (w o s c d)
	`(progn
		(unless (eql ,s ,d) (emit-xor-rr ,d ,d))
		(emit-ir ,w ,o ,s ,c ,d)
		(if (eql ,s ,d) (emit-ux ,(elem-get w '(7 15 31)) ,d ,d))))

(defmacro emit-rd (o s d1 d2)
	`(emit-int (+ ,o (<< ,d2 16) (<< ,d1 5) ,s)))

(defmacro emit-dr (o s1 s2 d)
	`(emit-int (+ ,o (<< ,s2 16) (<< ,s1 5) ,d)))

(defmacro emit-dru (o x s1 s2 d)
	`(progn
		(and (not (eql ,s1 ,d)) (not (eql ,s2 ,d)) (emit-xor-rr ,d ,d))
		(emit-dr ,o ,s1 ,s2 ,d)
		(if (or (eql ,s1 ,d) (eql ,s2 ,d)) (emit-ux ,x ,d ,d))))

(defmacro emit-branch (o l)
	`(cond
		((<= -0x100000 (defq ,(defq c (gensym)) (- ,l *pc*)) 0xffffc)
			(emit-int (+ 0x54000000 (<< (logand ,c 0x1ffffc) 3) ,o)))
		(:t (throw "emit-branch constant out of range !" ,c))))

(defun emit-cpy-ri (s d c)
	(when (eql s (const (emit-native-reg? :rsp)))
		(emit-cpy-rr (const (emit-native-reg? :rsp)) (const (emit-native-reg? :r29)))
		(setq s (const (emit-native-reg? :r29))))
	(emit-ri 3 0xf8000000 s d c))

(defun emit-push (&rest b)
	(cond
		((= 0 (defq l (length b))))
		((= 1 l)
			(emit-int (+ 0xf81f0c00 (const (<< (emit-native-reg? :rsp) 5)) (elem-get 0 b))))
		((= 2 l)
			(emit-int (+ 0xa9bf0000
				(<< (elem-get 0 b) 10) (const (<< (emit-native-reg? :rsp) 5)) (elem-get 1 b))))
		(:t  (emit-sub-cr (align (* +ptr_size l) stack_align) (const (emit-native-reg? :rsp)))
			(defq _ l)
			(while (>= (setq _ (- _ 2)) 0)
				(emit-int (+ 0xa9000000 (<< (- l _ 2) 15)
					(<< (elem-get _ b) 10) (const (<< (emit-native-reg? :rsp) 5)) (elem-get (inc _) b))))
			(if (/= -2 _) (emit-cpy-ri (elem-get 0 b) (const (emit-native-reg? :rsp)) (* +ptr_size (dec l)))))))

(defun emit-pop (&rest b)
	(cond
		((= 0 (defq l (length b))))
		((= 1 l)
			(emit-int (+ 0xf8410400 (const (<< (emit-native-reg? :rsp) 5)) (elem-get 0 b))))
		((= 2 l)
			(emit-int (+ 0xa8c10000
				(<< (elem-get 0 b) 10) (const (<< (emit-native-reg? :rsp) 5)) (elem-get 1 b))))
		(:t  (defq _ l)
			(while (>= (setq _ (- _ 2)) 0)
				(emit-int (+ 0xa9400000 (<< (- l _ 2) 15)
					(<< (elem-get _ b) 10) (const (<< (emit-native-reg? :rsp) 5)) (elem-get (inc _) b))))
			(if (/= -2 _) (emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (dec l)) (elem-get 0 b)))
			(emit-add-cr (align (* +ptr_size l) stack_align) (const (emit-native-reg? :rsp))))))

(defun emit-beq-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0x0 l))
(defun emit-bne-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0x1 l))
(defun emit-bge-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0xa l))
(defun emit-blt-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0xb l))
(defun emit-bgt-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0xc l))
(defun emit-ble-cr (c d l m) (emit-cmp-cr c d) (emit-branch 0xd l))

(defun emit-beq-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0x0 l))
(defun emit-bne-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0x1 l))
(defun emit-bge-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0xa l))
(defun emit-blt-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0xb l))
(defun emit-bgt-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0xc l))
(defun emit-ble-rr (s d l m) (emit-cmp-rr s d) (emit-branch 0xd l))

(defun emit-seq-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x54000060) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sne-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x54000061) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-slt-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006b) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sle-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006d) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sgt-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006c) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sge-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006a) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))

(defun emit-seq-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x54000060) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sne-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x54000061) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-slt-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006b) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sle-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006d) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sgt-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006c) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sge-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006a) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))

(defun emit-call (l)
	(emit-push (const (emit-native-reg? :r30)))
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emit-int (+ 0x94000000 (>> (logand c 0xfffffff) 2))))
		(:t (throw "emit-call constant out of range !" c)))
	(emit-pop (const (emit-native-reg? :r30))))

(defun emit-cpy-pr (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xffffc)
			(emit-int (+ 0x58000000 (<< (logand c 0x1fffff) 3) d)))
		(:t (throw "emit-cpy-pr constant out of range !" c))))

(defun emit-call-r (r)
	(emit-push (const (emit-native-reg? :r30)))
	(emit-int (+ 0xd63f0000 (<< r 5)))
	(emit-pop (const (emit-native-reg? :r30))))

(defun emit-call-i (d c)
	(emit-cpy-ir d c (const (emit-native-reg? :r29)))
	(emit-call-r (const (emit-native-reg? :r29))))

(defun emit-call-p (l)
	(emit-cpy-pr l (const (emit-native-reg? :r29)))
	(emit-call-r (const (emit-native-reg? :r29))))

(defun emit-jmp (l d)
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emit-int (+ 0x14000000 (>> (logand c 0xfffffff) 2))))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-jmp-r (r)
	(emit-int (+ 0xd61f0000 (<< r 5))))

(defun emit-jmp-i (d c)
	(emit-cpy-ir d c (const (emit-native-reg? :r29)))
	(emit-jmp-r (const (emit-native-reg? :r29))))

(defun emit-jmp-p (l)
	(emit-cpy-pr l (const (emit-native-reg? :r29)))
	(emit-jmp-r (const (emit-native-reg? :r29))))

(defun emit-lea-i (s c d)
	(cond
		((and (= c 0) (eql s d)))
		((<= -0x1000 c 0xfff)
			(if (>= c 0)
				(emit-int (+ 0x91000000 (<< (logand c 0xfff) 10) (<< s 5) d))
				(emit-int (+ 0xd1000000 (<< (logand (neg c) 0xfff) 10) (<< s 5) d))))
		(:t (throw "emit-lea-i constant out of range !" c))))

(defun emit-lea-d (s1 s2 d) (emit-rrr 0x8b000000 s1 s2 d))

(defun emit-lea-p (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(emit-int (+ 0x10000000 (<< (logand c 0x3) 29) (<< (logand c 0x1ffffc) 3) d)))
		(:t (throw "emit-lea-p constant out of range !" c))))

(defun emit-cpy-ir-b (s c d) (emit-ir 0 0x38800000 s c d))
(defun emit-cpy-ir-s (s c d) (emit-ir 1 0x78800000 s c d))
(defun emit-cpy-ir-i (s c d) (emit-ir 2 0xb8800000 s c d))

(defun emit-cpy-ri-b (s d c) (emit-ri 0 0x38000000 s d c))
(defun emit-cpy-ri-s (s d c) (emit-ri 1 0x78000000 s d c))
(defun emit-cpy-ri-i (s d c) (emit-ri 2 0xb8000000 s d c))

(defun emit-cpy-ir-ub (s c d) (emit-iru 0 0x38400000 s c d))
(defun emit-cpy-ir-us (s c d) (emit-iru 1 0x78400000 s c d))
(defun emit-cpy-ir-ui (s c d) (emit-ir 2 0xb8400000 s c d))

(defun emit-cpy-rd (s d1 d2) (emit-rd 0xf8206800 s d2 d1))
(defun emit-cpy-rd-b (s d2 d1) (emit-rd 0x38206800 s d2 d1))
(defun emit-cpy-rd-s (s d2 d1) (emit-rd 0x78206800 s d2 d1))
(defun emit-cpy-rd-i (s d2 d1) (emit-rd 0xb8206800 s d2 d1))

(defun emit-cpy-dr (s1 s2 d) (emit-dr 0xf8606800 s2 s1 d))
(defun emit-cpy-dr-b (s1 s2 d) (emit-dr 0x38a06800 s2 s1 d))
(defun emit-cpy-dr-s (s1 s2 d) (emit-dr 0x78a06800 s2 s1 d))
(defun emit-cpy-dr-i (s1 s2 d) (emit-dr 0xb8a06800 s2 s1 d))
(defun emit-cpy-dr-ub (s1 s2 d) (emit-dru 0x38606800 7 s1 s2 d))
(defun emit-cpy-dr-us (s1 s2 d) (emit-dru 0x78606800 15 s1 s2 d))
(defun emit-cpy-dr-ui (s1 s2 d) (emit-dru 0xb8606800 31 s1 s2 d))

(defun emit-lnot-rr (r d)
	(emit-cmp-cr 0 d)
	(emit-int (+ 0x9a9f17e0 d)))

(defun emit-land-rr (s d)
	(emit-cmp-cr 0 d)
	(emit-int (+ 0xfa401804 (<< s 5)))
	(emit-int (+ 0x9a9f07e0 d)))

(defun emit-div-rrr (s d1 d2)
	(emit-int (+ 0x9ac00c00 (<< s 16) (<< d2 5) (const (emit-native-reg? :r29))))
	(emit-int (+ 0x9b008000 (const (<< (emit-native-reg? :r29) 16)) (<< d2 10) (<< s 5) d1))
	(emit-cpy-rr (const (emit-native-reg? :r29)) d2))

(defun emit-div-rrr-u (s d1 d2)
	(emit-int (+ 0x9ac00800 (<< s 16) (<< d2 5) (const (emit-native-reg? :r29))))
	(emit-int (+ 0x9b008000 (const (<< (emit-native-reg? :r29) 16)) (<< d2 10) (<< s 5) d1))
	(emit-cpy-rr (const (emit-native-reg? :r29)) d2))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-free (c) (emit-add-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-ret () (emit-int 0xd65f03c0))
(defun emit-sync (n) (emit-int 0xd5033fbf))
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
	(emit-cpy-ir b c (const (emit-native-reg? :r29)))
	(emit-int (const (+ 0xd63f0000 (<< (emit-native-reg? :r29) 5))))
	(cond
		((= 1 (defq l (length x)))
			(emit-pop (const (emit-native-reg? :r30))))
		((= 2 l)
			(emit-pop (const (emit-native-reg? :r30)) (const (emit-native-reg? :r29))))
		(:t  (emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (dec l)) (const (emit-native-reg? :r30)))
			(emit-free (* +ptr_size l))))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))
