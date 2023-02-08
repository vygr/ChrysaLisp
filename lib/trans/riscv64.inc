;;;;;;;;;;;;;;;;;;;;;;;;
; RISCV64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defcvar 'stack_align 16 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r30))
(defun emit-native-reg? (r)
	(find r '(:r29 :r30 :rsp							;zero, ra, sp (x0-x2)
		:r15 :r16										;gp, tp (x3-x4)
		:r17 :r18 :r19									;t0-t1, t2 (x5-x7)
		:r20 :r21										;s0-s1 (x8-x9) saved
		:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7					;a0-a7 (x10-x17)
		:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r22 :r23 :r24	;s2-s11 (x18-x27) saved
		:r25 :r26 :r27 :r28)))							;t3-t6 (x28-x31)
(each (# (defcvar %0 %0)) '(:r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30))

(defq +riscv64_rlnk (emit-native-reg? :r30)
	+riscv64_rzero (emit-native-reg? :r29)
	+riscv64_rtmp (emit-native-reg? :r28)
	+riscv64_rtmp1 (emit-native-reg? :r27)
	+riscv64_rsp (emit-native-reg? :rsp))

(defmacro riscv64-r (o rd rs1 rs2 f3 f7)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12) (<< ,f7 25))))

(defmacro riscv64-i (o rd rs1 f3 imm12)
	`(emit-int (+ ,o (<< ,rd 7) (<< ,rs1 15) (<< ,f3 12) (<< ,imm12 20))))

(defmacro riscv64-s (o rs1 rs2 f3 imm12)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1f ,imm12) 7) (<< (logand 0xfe0 ,imm12) 20))))

(defmacro riscv64-b (o rs1 rs2 f3 imm13)
	`(emit-int (+ ,o (<< ,rs1 15) (<< ,rs2 20) (<< ,f3 12)
		(<< (logand 0x1e ,imm13) 7) (<< (logand 0x7e0 ,imm13) 20)
		(>> (logand 0x800 ,imm13) 4) (<< (logand 0x1000 ,imm13) 19))))

(defmacro riscv64-u (o rd imm32)
	`(emit-int (+ ,o (<< ,rd 7) (logand ,imm32 0xfffff000))))

(defmacro riscv64-j (o rd imm21)
	`(emit-int (+ ,o (<< ,rd 7)
		(<< (logand 0x7fe ,imm21) 20) (<< (logand 0x800 ,imm21) 9)
		(logand 0xff000 ,imm21) (<< (logand 0x100000 ,imm21) 11))))

(defmacro riscv64-within (c n)
	`(<= ,(neg (<< 1 (- n 1))) ,c ,(>> -1 (- 65 n))))

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
	(riscv64-i 0x13 r r 0x5 (+ c 0x40000000)))

(defun emit-cpy-rr (s d)
	(cond
		((eql s d))
		(:t (riscv64-i 0x13 d s 0x0 0x0))))

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
	(riscv64-r 0x33 +riscv64_rtmp d2 s 0x4 0x1)
	(riscv64-r 0x33 d1 d2 s 0x6 0x1)
	(emit-cpy-rr +riscv64_rtmp d2))

(defun emit-div-rrr-u (s d1 d2)
	(riscv64-r 0x33 +riscv64_rtmp d2 s 0x5 0x1)
	(riscv64-r 0x33 d1 d2 s 0x7 0x1)
	(emit-cpy-rr +riscv64_rtmp d2))

(defun riscv64-cr (c r)
	(cond
		((riscv64-within c 12)
			(riscv64-i 0x13 r +riscv64_rzero 0x0 c))
		((riscv64-within c 32)
			(if (= (logand 0x800 c) 0)
				(defq u c)
				(defq u (+ 0x1000 c)))
			(riscv64-u 0x37 r u)
			(if (/= (logand c 0xfff) 0) (riscv64-i 0x13 r r 0x0 c)))
		(:t (throw "riscv64-cr constant out of range !" c))))

(defun emit-cpy-cr (c r)
	(cond
		((riscv64-within c 32)
			(riscv64-cr c r))
		(:t (defq l (>>> (<< c 32) 32) u (>>> c 32))
			(if (< l 0) (setq u (>>> (<< (inc u) 32) 32)))
			(cond
				((= l 0)
					(riscv64-cr u r)
					(emit-shl-cr 32 r))
				(:t (riscv64-cr u +riscv64_rtmp1)
					(riscv64-cr l r)
					(emit-shl-cr 32 +riscv64_rtmp1)
					(emit-add-rr +riscv64_rtmp1 r))))))

(defun emit-add-cr (c r)
	(cond
		((= c 0))
		((riscv64-within c 12)
			(riscv64-i 0x13 r r 0x0 c))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-add-rr +riscv64_rtmp r))))

(defun emit-sub-cr (c r) (emit-add-cr (neg c) r))

(defun emit-xor-cr (c r)
	(cond
		((= c 0))
		((riscv64-within c 12)
			(riscv64-i 0x13 r r 0x4 c))
		(:t (throw "emit-xor-cr constant out of range !" c))))

(defun emit-or-cr (c r)
	(cond
		((= c 0))
		((riscv64-within c 12)
			(riscv64-i 0x13 r r 0x6 c))
		(:t (throw "emit-or-cr constant out of range !" c))))

(defun emit-and-cr (c r)
	(cond
		((= c -1))
		((riscv64-within c 12)
			(riscv64-i 0x13 r r 0x7 c))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-and-rr +riscv64_rtmp r))))

(defun emit-mul-cr (c r)
	(cond
		((= c 1))
		((= c 0)
			(emit-xor-rr r r))
		((= c -1)
			(riscv64-r 0x33 r +riscv64_rzero r 0x0 0x20))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-mul-rr +riscv64_rtmp r))))

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s +riscv64_rtmp)
		(emit-cpy-rr d s)
		(emit-cpy-rr +riscv64_rtmp d)))

(defun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))

(defun riscv64-branch (o d s l)
	(cond
		((riscv64-within (defq c (- l *pc*)) 13)
			(riscv64-b 0x63 d s o c))
		(:t (throw "riscv64-branch constant out of range !" c))))

(defun emit-beq-rr (s d l m) (riscv64-branch 0x0 d s l))
(defun emit-bne-rr (s d l m) (riscv64-branch 0x1 d s l))
(defun emit-blt-rr (s d l m) (riscv64-branch 0x4 d s l))
(defun emit-bge-rr (s d l m) (riscv64-branch 0x5 d s l))
(defun emit-ble-rr (s d l m) (riscv64-branch 0x5 s d l))
(defun emit-bgt-rr (s d l m) (riscv64-branch 0x4 s d l))

(defun emit-beq-cr (c d l m)
	(cond
		((= c 0)
			(emit-beq-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-beq-rr +riscv64_rtmp d l m))))
(defun emit-bne-cr (c d l m)
	(cond
		((= c 0)
			(emit-bne-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-bne-rr +riscv64_rtmp d l m))))
(defun emit-blt-cr (c d l m)
	(cond
		((= c 0)
			(emit-blt-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-blt-rr +riscv64_rtmp d l m))))
(defun emit-bge-cr (c d l m)
	(cond
		((= c 0)
			(emit-bge-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-bge-rr +riscv64_rtmp d l m))))
(defun emit-ble-cr (c d l m)
	(cond
		((= c 0)
			(emit-ble-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-ble-rr +riscv64_rtmp d l m))))
(defun emit-bgt-cr (c d l m)
	(cond
		((= c 0)
			(emit-bgt-rr +riscv64_rzero d l m))
		(:t (emit-cpy-cr c +riscv64_rtmp)
			(emit-bgt-rr +riscv64_rtmp d l m))))

(defun riscv64-set (o d s c)
	(riscv64-b 0x63 d s o c))

(defun emit-seq-rr (s d)
	(riscv64-set 0x0 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sne-rr (s d)
	(riscv64-set 0x1 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-slt-rr (s d)
	(riscv64-set 0x4 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sge-rr (s d)
	(riscv64-set 0x5 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sle-rr (s d)
	(riscv64-set 0x5 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sgt-rr (s d)
	(riscv64-set 0x4 d s 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))

(defun emit-seq-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x0 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sne-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x1 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-slt-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x4 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sge-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x5 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sle-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x5 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))
(defun emit-sgt-cr (c d)
	(emit-cpy-cr c +riscv64_rtmp)
	(riscv64-set 0x4 d +riscv64_rtmp 12)
	(emit-xor-rr d d)
	(riscv64-j 0x6f +riscv64_rzero 8)
	(emit-or-cr -1 d))

(defun emit-lnot-rr (r d)
	(riscv64-i 0x13 d d 0x3 0x1))

(defun emit-land-rr (s d)
	(riscv64-r 0x33 +riscv64_rtmp s +riscv64_rzero 0x3 0x0)
	(riscv64-r 0x33 d d +riscv64_rzero 0x3 0x0)
	(riscv64-r 0x33 d d +riscv64_rtmp 0x7 0x0))

(defun riscv64-dr (o s i d)
	(riscv64-r 0x33 +riscv64_rtmp s i 0x0 0x0)
	(riscv64-i 0x3 d +riscv64_rtmp o 0))

(defun riscv64-rd (o s d i)
	(riscv64-r 0x33 +riscv64_rtmp d i 0x0 0x0)
	(riscv64-i 0x23 s +riscv64_rtmp o 0))

(defun riscv64-ir (o s i d)
	(cond
		((riscv64-within i 12)
			(riscv64-i 0x3 d s o i))
		(:t (emit-cpy-cr i +riscv64_rtmp)
			(riscv64-dr o s +riscv64_rtmp d))))

(defun emit-cpy-ir-b (s i d) (riscv64-ir 0x0 s i d))
(defun emit-cpy-ir-s (s i d) (riscv64-ir 0x1 s i d))
(defun emit-cpy-ir-i (s i d) (riscv64-ir 0x2 s i d))
(defun emit-cpy-ir (s i d) (riscv64-ir 0x3 s i d))
(defun emit-cpy-ir-ub (s i d) (riscv64-ir 0x4 s i d))
(defun emit-cpy-ir-us (s i d) (riscv64-ir 0x5 s i d))
(defun emit-cpy-ir-ui (s i d) (riscv64-ir 0x6 s i d))

(defun riscv64-ri (o s d i)
	(cond
		((riscv64-within i 12)
			(riscv64-s 0x23 d s o i))
		(:t (emit-cpy-cr i +riscv64_rtmp)
			(riscv64-rd o s d +riscv64_rtmp))))

(defun emit-cpy-ri-b (s d i) (riscv64-ri 0x0 s d i))
(defun emit-cpy-ri-s (s d i) (riscv64-ri 0x1 s d i))
(defun emit-cpy-ri-i (s d i) (riscv64-ri 0x2 s d i))
(defun emit-cpy-ri (s d i) (riscv64-ri 0x3 s d i))

(defun emit-cpy-dr-b (s i d) (riscv64-dr 0x0 s i d))
(defun emit-cpy-dr-s (s i d) (riscv64-dr 0x1 s i d))
(defun emit-cpy-dr-i (s i d) (riscv64-dr 0x2 s i d))
(defun emit-cpy-dr (s i d) (riscv64-dr 0x3 s i d))
(defun emit-cpy-dr-ub (s i d) (riscv64-dr 0x4 s i d))
(defun emit-cpy-dr-us (s i d) (riscv64-dr 0x5 s i d))
(defun emit-cpy-dr-ui (s i d) (riscv64-dr 0x6 s i d))

(defun emit-cpy-rd-b (s d i) (riscv64-rd 0x0 s d i))
(defun emit-cpy-rd-s (s d i) (riscv64-rd 0x1 s d i))
(defun emit-cpy-rd-i (s d i) (riscv64-rd 0x2 s d i))
(defun emit-cpy-rd (s d i) (riscv64-rd 0x3 s d i))

(defun emit-lea-i (s i d)
	(cond
		((and (= i 0) (eql s d)))
		((riscv64-within i 12)
			(riscv64-i 0x13 d s 0x0 i))
		(:t (throw "emit-lea-i constant out of range !" i))))

(defun emit-lea-d (s i d) (riscv64-r 0x33 d s i 0x0 0x0))

(defun emit-push (&rest b)
	(when (/= 0 (defq l (length b)))
		(emit-sub-cr (align (* +ptr_size l) stack_align) +riscv64_rsp)
		(defq _ l)
		(while (>= (setq _ (dec _)) 0)
			(emit-cpy-ri (elem-get _ b) +riscv64_rsp (* +ptr_size (- l _ 1))))))

(defun emit-pop (&rest b)
	(when (/= 0 (defq l (length b)))
		(defq _ l)
		(while (>= (setq _ (dec _)) 0)
			(emit-cpy-ir +riscv64_rsp (* +ptr_size (- l _ 1)) (elem-get _ b)))
		(emit-add-cr (align (* +ptr_size l) stack_align) +riscv64_rsp)))

(defun emit-call (l)
	(emit-push +riscv64_rlnk)
	(cond
		((riscv64-within (defq c (- l *pc*)) 21)
			(riscv64-j 0x6f +riscv64_rlnk c))
		(:t (throw "emit-call constant out of range !" c)))
	(emit-pop +riscv64_rlnk))

(defun emit-jmp (l m)
	(cond
		((riscv64-within (defq c (- l *pc*)) 21)
			(riscv64-j 0x6f +riscv64_rzero c))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-call-r (r)
	(emit-push +riscv64_rlnk)
	(riscv64-i 0x67 +riscv64_rlnk r 0x0 0x0)
	(emit-pop +riscv64_rlnk))

(defun emit-jmp-r (r)
	(riscv64-i 0x67 +riscv64_rzero r 0x0 0x0))

(defun emit-call-i (s i)
	(emit-cpy-ir s i +riscv64_rtmp)
	(emit-call-r +riscv64_rtmp))

(defun emit-jmp-i (s i)
	(emit-cpy-ir s i +riscv64_rtmp)
	(emit-jmp-r +riscv64_rtmp))

(defun emit-lea-p (l d)
	(cond
		((riscv64-within (defq c (- l *pc*)) 32)
			(if (= (logand 0x800 c) 0)
				(defq u c)
				(defq u (+ 0x1000 c)))
			(riscv64-u 0x17 d u)
			(if (/= (logand c 0xfff) 0) (riscv64-i 0x13 d d 0x0 c)))
		(:t (throw "emit-lea-p constant out of range !" c))))

(defun emit-cpy-pr (l d)
	(emit-lea-p l +riscv64_rtmp1)
	(emit-cpy-ir +riscv64_rtmp1 0 d))

(defun emit-call-p (l)
	(emit-cpy-pr l +riscv64_rtmp)
	(emit-call-r +riscv64_rtmp))

(defun emit-jmp-p (l)
	(emit-cpy-pr l +riscv64_rtmp)
	(emit-jmp-r +riscv64_rtmp))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) +riscv64_rsp))
(defun emit-free (c) (emit-add-cr (align c stack_align) +riscv64_rsp))
(defun emit-ret () (emit-jmp-r +riscv64_rlnk))
(defun emit-sync (n) (emit-int 0x0ff0000f))
(defun emit-brk (n))

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-and-cr (neg stack_align) s)
	(emit-sub-cr tk_state_size s)
	(emit-cpy-ri f s (- tk_state_size +ptr_size))
	(emit-cpy-ri x s (- tk_state_size (* +ptr_size 2)))
	(emit-lea-p (defcvar (label-sym 'riscv_start) (+ *pc* 16)) f)
	(emit-cpy-ri f s 0)
	(emit-jmp (defcvar (label-sym 'riscv_exit) (+ *pc* 12)) 0)
;riscv_start
	(emit-cpy-rr (const (emit-native-reg? :r1)) +riscv64_rlnk)
	(emit-jmp-r (const (emit-native-reg? :r0))))
;riscv_exit

(case *abi*
(RISCV64
(defun emit-call-abi (r b c n &rest x)
	(defq x (cat `(,+riscv64_rlnk) x) l (length x))
	(apply (const emit-push) x)
	(emit-cpy-ir b c +riscv64_rtmp)
	(riscv64-i 0x67 +riscv64_rlnk +riscv64_rtmp 0x0 0x0)
	(emit-cpy-ir +riscv64_rsp (* +ptr_size (dec l)) +riscv64_rlnk)
	(emit-free (* +ptr_size l))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))
