;;;;;;;;;;;;;;;;;;;;
; ARM Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defmacro emit-ux (x s d)
	`(emit-int (add 0xd3400000 (bit-shl ,x 10) (bit-shl (reg ,s) 5) (reg ,d))))

(defmacro emit-bf (o x y s d)
	`(emit-int (add ,o (bit-shl ,x 16) (bit-shl ,y 10) (bit-shl (reg ,s) 5) (reg ,d))))

(defmacro emit-rrr (o m n d)
	`(emit-int (add ,o (bit-shl (reg ,m) 16) (bit-shl (reg ,n) 5) (reg ,d))))

(defmacro emit-ri (o s d c)
	`(cond
		((le -0x100 ,c 0xff)
			(emit-int (add ,o (bit-shl (bit-and ,c 0x1ff) 12) (bit-shl (reg ,d) 5) (reg ,s))))
		((le 0 ,c 0xfff)
			(emit-int (add 0x1000000 ,o (bit-shl ,c 10) (bit-shl (reg ,d) 5) (reg ,s))))
		(t (throw "emit-ri/ir constant out of range" ,c))))

(defmacro emit-ir (o s c d)
	`(emit-ri ,o ,d ,s ,c))

(defmacro emit-iru (o x s c d)
	`(progn
		(unless (eql ,s ,d) (emit-xor-rr ,d ,d))
		(emit-ir ,o ,s ,c ,d)
		(if (eql ,s ,d) (emit-ux ,x ,d ,d))))

(defmacro emit-rd (o s d1 d2)
	`(emit-int (add ,o (bit-shl (reg ,d2) 16) (bit-shl (reg ,d1) 5) (reg ,s))))

(defmacro emit-dr (o s1 s2 d)
	`(emit-int (add ,o (bit-shl (reg ,s2) 16) (bit-shl (reg ,s1) 5) (reg ,d))))

(defmacro emit-dru (o x s1 s2 d)
	`(progn
		(and (not (eql ,s1 ,d)) (not (eql ,s2 ,d)) (emit-xor-rr ,d ,d))
		(emit-dr ,o ,s1 ,s2 ,d)
		(if (or (eql ,s1 ,d) (eql ,s2 ,d)) (emit-ux ,x ,d ,d))))

(defmacro emit-branch (o l)
	`(cond
		((le -0x100000 (defq ,(defq c (gensym)) (sub ,l *pc*)) 0xffffc)
			(emit-int (add 0x54000000 (bit-shl (bit-and ,c 0x1ffffc) 3) ,o)))
		(t (throw "emit-branch constant out of range" ,c))))

(defcfun reg (r)
	(find r '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
		r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 rsp)))

(each (lambda (_) (defcvar _ _)) '(r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30))
(defcvar 'stack_align 16 'stack_state '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r30))

(defcfun stack-init ()
	(defq tk_state_size (mul ptr_size (length stack_state)))
	(vp-sub-cr tk_state_size r1)
	(f-bind 'sys_task 'stop r2)
	(vp-cpy-ri r4 r1 (sub tk_state_size ptr_size))
	(vp-cpy-ri r2 r1 (sub tk_state_size (mul ptr_size 2)))
	(vp-lea-p 'start r4)
	(vp-cpy-ri r4 r1 0)
	(goto 'exit)
(vp-label 'start)
	(vp-cpy-rr r1 r30)
	(vp-jmp-r r0)
(vp-label 'exit))

(defcfun emit-alloc (c) (emit-sub-cr (align c stack_align) rsp))
(defcfun emit-free (c) (emit-add-cr (align c stack_align) rsp))
(defcfun emit-ret () (emit-int 0xd65f03c0))

(defcfun emit-push (&rest b)
	(cond
		((eq 1 (defq l (length b)))
			(emit-int (add 0xf81f0c00 (bit-shl (reg rsp) 5) (reg (elem 0 b)))))
		((eq 2 l)
			(emit-int (add 0xa9bf0000
				(bit-shl (reg (elem 0 b)) 10) (bit-shl (reg rsp) 5) (reg (elem 1 b)))))
		(t
			(emit-sub-cr (align (mul ptr_size l) stack_align) rsp)
			(defq _ l)
			(while (ge (setq _ (sub _ 2)) 0)
				(emit-int (add 0xa9000000 (bit-shl (sub l _ 2) 15)
					(bit-shl (reg (elem _ b)) 10) (bit-shl (reg rsp) 5) (reg (elem (inc _) b)))))
			(if (ne -2 _) (emit-cpy-ri (elem 0 b) rsp (mul ptr_size (dec l)))))))

(defcfun emit-pop (&rest b)
	(cond
		((eq 1 (defq l (length b)))
			(emit-int (add 0xf8410400 (bit-shl (reg rsp) 5) (reg (elem 0 b)))))
		((eq 2 l)
			(emit-int (add 0xa8c10000
				(bit-shl (reg (elem 0 b)) 10) (bit-shl (reg rsp) 5) (reg (elem 1 b)))))
		(t
			(defq _ l)
			(while (ge (setq _ (sub _ 2)) 0)
				(emit-int (add 0xa9400000 (bit-shl (sub l _ 2) 15)
					(bit-shl (reg (elem _ b)) 10) (bit-shl (reg rsp) 5) (reg (elem (inc _) b)))))
			(if (ne -2 _) (emit-cpy-ir rsp (mul ptr_size (dec l)) (elem 0 b)))
			(emit-add-cr (align (mul ptr_size l) stack_align) rsp))))

(defcfun emit-beq (l d) (emit-branch 0x0 l))
(defcfun emit-bne (l d) (emit-branch 0x1 l))
(defcfun emit-bge (l d) (emit-branch 0xa l))
(defcfun emit-blt (l d) (emit-branch 0xb l))
(defcfun emit-bgt (l d) (emit-branch 0xc l))
(defcfun emit-ble (l d) (emit-branch 0xd l))

(defcfun emit-syscall ())

(defcfun emit-call (l)
	(emit-push r30)
	(defq c (sub l *pc*))
	(cond
		((le -0x8000000 c 0x7fffffc)
			(emit-int (add 0x94000000 (bit-shr (bit-and c 0xfffffff) 2))))
		(t (throw "emit-call constant out of range" c)))
	(emit-pop r30))

(defcfun emit-call-r (r)
	(emit-push r30)
	(emit-int (add 0xd63f0000 (bit-shl (reg r) 5)))
	(emit-pop r30))

(defcfun emit-call-i (d c)
	(emit-cpy-ir d c r29)
	(emit-call-r r29))

(defcfun emit-call-p (l)
	(emit-cpy-pr l r29)
	(emit-call-r r29))

(defcfun emit-jmp (l d)
	(defq c (sub l *pc*))
	(cond
		((le -0x8000000 c 0x7fffffc)
			(emit-int (add 0x14000000 (bit-shr (bit-and c 0xfffffff) 2))))
		(t (throw "emit-jmp constant out of range" c))))

(defcfun emit-jmp-r (r)
	(emit-int (add 0xd61f0000 (bit-shl (reg r) 5))))

(defcfun emit-jmp-i (d c)
	(emit-cpy-ir d c r29)
	(emit-jmp-r r29))

(defcfun emit-jmp-p (l)
	(emit-cpy-pr l r29)
	(emit-jmp-r r29))

(defcfun emit-lea-i (s c d)
	(cond
		((le -0x1000 c 0xfff)
			(if (ge c 0)
				(emit-int (add 0x91000000 (bit-shl (bit-and c 0xfff) 10) (bit-shl (reg s) 5) (reg d)))
				(emit-int (add 0xd1000000 (bit-shl (bit-and (neg c) 0xfff) 10) (bit-shl (reg s) 5) (reg d)))))
		(t (throw "emit-lea-i constant out of range" c))))

(defcfun emit-lea-d (s1 s2 d) (emit-rrr 0x8b000000 s1 s2 d))

(defcfun emit-lea-p (l d)
	(defq c (sub l *pc*))
	(cond
		((le -0x100000 c 0xffffc)
			(emit-int (add 0x10000000 (bit-shl (bit-and c 0x1fffff) 3) (reg d))))
		(t (throw "emit-lea-p constant out of range" c))))

(defcfun emit-cpy-cr (c r)
	(defq r (reg r) x (bit-and c 0xffff00000000) y (bit-and c 0xffff0000) z (bit-and c 0xffff))
	(if (lt c 0)
		(cond
			((ge c -0x10000)
				(emit-int (add 0x92800000 (bit-shl (bit-not c) 5) r)))
			((ge c -0x100000000)
				(emit-int (add 0x92a00000 (bit-shr (bit-and (bit-not c) 0xffff0000) 11) r))
				(if (ne z 0xffff) (emit-int (add 0xf2800000 (bit-shl z 5) r))))
			((ge c -0x1000000000000)
				(emit-int (add 0x92c00000 (bit-shr (bit-and (bit-not c) 0xffff00000000) 27) r))
				(if (ne y 0xffff0000) (emit-int (add 0xf2a00000 (bit-shr y 11) r)))
				(if (ne z 0xffff) (emit-int (add 0xf2800000 (bit-shl z 5) r))))
			(t
				(emit-int (add 0x92e00000 (bit-shr (bit-and (bit-not c) 0xffff000000000000) 43) r))
				(if (ne x 0xffff00000000) (emit-int (add 0xf2c00000 (bit-shr x 27) r)))
				(if (ne y 0xffff0000) (emit-int (add 0xf2a00000 (bit-shr y 11) r)))
				(if (ne z 0xffff) (emit-int (add 0xf2800000 (bit-shl z 5) r)))))
		(cond
			((le c 0xffff)
				(emit-int (add 0xd2800000 (bit-shl c 5) r)))
			((le c 0xffffffff)
				(emit-int (add 0xd2a00000 (bit-shr (bit-and c 0xffff0000) 11) r))
				(if (ne z 0) (emit-int (add 0xf2800000 (bit-shl z 5) r))))
			((le c 0xffffffffffff)
				(emit-int (add 0xd2c00000 (bit-shr (bit-and c 0xffff00000000) 27) r))
				(if (ne y 0) (emit-int (add 0xf2a00000 (bit-shr y 11) r)))
				(if (ne z 0) (emit-int (add 0xf2800000 (bit-shl z 5) r))))
			(t
				(emit-int (add 0xd2e00000 (bit-shr (bit-and c 0xffff000000000000) 43) r))
				(if (ne x 0) (emit-int (add 0xf2c00000 (bit-shr x 27) r)))
				(if (ne y 0) (emit-int (add 0xf2a00000 (bit-shr y 11) r)))
				(if (ne z 0) (emit-int (add 0xf2800000 (bit-shl z 5) r)))))))

(defcfun emit-cpy-ir (s c d)
	(if (eql d rsp)
		(progn
			(emit-ir 0xf8400000 s c r29)
			(emit-cpy-rr r29 rsp))
		(emit-ir 0xf8400000 s c d)))

(defcfun emit-cpy-ir-b (s c d) (emit-ir 0x38800000 s c d))
(defcfun emit-cpy-ir-s (s c d) (emit-ir 0x78800000 s c d))
(defcfun emit-cpy-ir-i (s c d) (emit-ir 0xb8800000 s c d))

(defcfun emit-cpy-ri (s d c)
	(when (eql s rsp)
		(emit-cpy-rr rsp r29)
		(setq s r29))
	(emit-ri 0xf8000000 s d c))

(defcfun emit-cpy-ri-b (s d c) (emit-ri 0x38000000 s d c))
(defcfun emit-cpy-ri-s (s d c) (emit-ri 0x78000000 s d c))
(defcfun emit-cpy-ri-i (s d c) (emit-ri 0xb8000000 s d c))

(defcfun emit-cpy-ir-ub (s c d) (emit-iru 0x38400000 7 s c d))
(defcfun emit-cpy-ir-us (s c d) (emit-iru 0x78400000 15 s c d))
(defcfun emit-cpy-ir-ui (s c d) (emit-iru 0xb8400000 31 s c d))

(defcfun emit-cpy-rd (s d1 d2) (emit-rd 0xf8206800 s d2 d1))
(defcfun emit-cpy-rd-b (s d2 d1) (emit-rd 0x38206800 s d2 d1))
(defcfun emit-cpy-rd-s (s d2 d1) (emit-rd 0x78206800 s d2 d1))
(defcfun emit-cpy-rd-i (s d2 d1) (emit-rd 0xb8206800 s d2 d1))

(defcfun emit-cpy-dr (s1 s2 d) (emit-dr 0xf8606800 s2 s1 d))
(defcfun emit-cpy-dr-b (s1 s2 d) (emit-dr 0x38a06800 s2 s1 d))
(defcfun emit-cpy-dr-s (s1 s2 d) (emit-dr 0x78a06800 s2 s1 d))
(defcfun emit-cpy-dr-i (s1 s2 d) (emit-dr 0xb8a06800 s2 s1 d))
(defcfun emit-cpy-dr-ub (s1 s2 d) (emit-dru 0x38606800 7 s1 s2 d))
(defcfun emit-cpy-dr-us (s1 s2 d) (emit-dru 0x78606800 15 s1 s2 d))
(defcfun emit-cpy-dr-ui (s1 s2 d) (emit-dru 0xb8606800 31 s1 s2 d))

(defcfun emit-cpy-rp (d l)
	(emit-lea-p l r29)
	(emit-cpy-ri d r29 0))

(defcfun emit-cpy-pr (l d)
	(defq c (sub l *pc*))
	(cond
		((le -0x100000 c 0xffffc)
			(emit-int (add 0x58000000 (bit-shl (bit-and c 0x1fffff) 3) (reg d))))
		(t (throw "emit-cpy-pr constant out of range" c))))

(defcfun emit-add-cr (c r)
	(cond
		((eq c 0))
		((lt c 0)
			(emit-sub-cr (neg c) r))
		((le c 0xffffff)
			(defq f (bit-and c 0xfff) c (bit-and c 0xfff000))
			(if (ne 0 f)
				(emit-int (add 0x91000000 (bit-shl f 10) (bit-shl (reg r) 5) (reg r))))
			(if (ne 0 c)
				(emit-int (add 0x91400000 (bit-shr c 2) (bit-shl (reg r) 5) (reg r)))))
		(t (throw "emit-add-cr constant out of range" c))))

(defcfun emit-sub-cr (c r)
	(cond
		((eq c 0))
		((lt c 0)
			(emit-add-cr (neg c) r))
		((le c 0xffffff)
			(defq f (bit-and c 0xfff) c (bit-and c 0xfff000))
			(if (ne 0 f)
				(emit-int (add 0xd1000000 (bit-shl f 10) (bit-shl (reg r) 5) (reg r))))
			(if (ne 0 c)
				(emit-int (add 0xd1400000 (bit-shr c 2) (bit-shl (reg r) 5) (reg r)))))
		(t (throw "emit-sub-cr constant out of range" c))))

(defcfun emit-cmp-cr (c r)
	(emit-cpy-cr c r29)
	(emit-cmp-rr r29 r))

(defcfun emit-and-cr (c r)
	(cond
		((eq c -1))
		((eq c 0)
			(emit-xor-rr r r))
		(t
			(emit-cpy-cr c r29)
			(emit-and-rr r29 r))))

(defcfun emit-or-cr (c r)
	(cond
		((eq c 0))
		((eq c -1)
			(emit-cpy-cr -1 r))
		(t
			(emit-cpy-cr c r29)
			(emit-or-rr r29 r))))

(defcfun emit-xor-cr (c r)
	(cond
		((eq c 0))
		(t
			(emit-cpy-cr c r29)
			(emit-xor-rr r29 r))))

(defcfun emit-cpy-rr (s d) (unless (eql s d) (emit-rrr 0x8b20e000 rsp s d)))
(defcfun emit-add-rr (s d) (if (eql s rsp) (emit-rrr 0x8b20e000 d s d) (emit-rrr 0x8b20e000 s d d)))

(defcfun emit-sub-rr (s d)
	(cond
		((eql s rsp)
			(throw "emit-sub-rr src can't be rsp !" (list s d)))
		(t (emit-rrr 0xcb20e000 s d d))))

(defcfun emit-cmp-rr (s d)
	(cond
		((eql s rsp)
			(throw "emit-cmp-rr src can't be rsp !" (list s d)))
		(t (emit-rrr 0xeb20e000 s d rsp))))

(defcfun emit-and-rr (s d)
	(cond
		((eql s d))
;		((or (eql s rsp) (eql d rsp))
;			(throw "emit-and-rr src/dst can't be rsp !" (list s d)))
		(t (emit-rrr 0x8a000000 s d d))))

(defcfun emit-or-rr (s d)
	(cond
		((eql s d))
		((or (eql s rsp) (eql d rsp))
			(throw "emit-or-rr src/dst can't be rsp !" (list s d)))
		(t (emit-rrr 0xaa000000 s d d))))

(defcfun emit-xor-rr (s d)
	(cond
		((or (eql s rsp) (eql d rsp))
			(throw "emit-xor-rr src/dst can't be rsp !" (list s d)))
		(t (emit-rrr 0xca000000 s d d))))

(defmacro emit-shift-cr (e o c x y r)
	`(cond
		((eq ,c 0))
		((eql ,r rsp)
			(throw ,(cat "emit-" e "-cr dst can't be rsp !") (list ,c ,r)))
		(t (emit-bf ,o ,x ,y ,r ,r))))

(defcfun emit-shl-cr (c r) (emit-shift-cr "shl" 0xd3400000 c (bit-and (sub 64 c) 63) (sub 63 c) r))
(defcfun emit-shr-cr (c r) (emit-shift-cr "shr" 0xd3400000 c c 63 r))
(defcfun emit-asr-cr (c r) (emit-shift-cr "asr" 0x93400000 c c 63 r))

(defmacro emit-shift-rr (e o s d)
	`(cond
		((or (eql ,s rsp) (eql ,d rsp))
			(throw ,(cat "emit-" e "-rr src/dst can't be rsp !") (list ,s ,d)))
		(t (emit-rrr ,o ,s ,d ,d))))

(defcfun emit-shl-rr (s d) (emit-shift-rr "shl" 0x9ac02000 s d))
(defcfun emit-shr-rr (s d) (emit-shift-rr "shr" 0x9ac02400 s d))
(defcfun emit-asr-rr (s d) (emit-shift-rr "asr" 0x9ac02800 s d))

(defcfun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s r29)
		(emit-cpy-rr d s)
		(emit-cpy-rr r29 d)))

(defcfun emit-mul-cr (c r)
	(cond
		((eq c 0)
			(emit-xor-rr r r))
		((eq c -1)
			(emit-int (add 0xcb000000 (bit-shl (reg r) 16) (bit-shl 31 5) (reg r))))
		(t
			(emit-cpy-cr c r29)
			(emit-mul-rr r29 r))))

(defcfun emit-mul-rr (s d)
	(if (or (eql s rsp) (eql d rsp))
		(throw "emit-mul-rr src/dst can't be rsp !" (list s d)))
	(emit-int (add 0x9b000000 (bit-shl (reg d) 16) (bit-shl 31 10) (bit-shl (reg s) 5) (reg d))))

(defcfun emit-ext-rr (s d)
	(cond
		((eql s d)
			(emit-asr-cr 63 d))
		(t
			(emit-cpy-rr s d)
			(emit-asr-cr 63 d))))

(defcfun emit-div-rrr (s d1 d2))
(defcfun emit-div-rrr-u (s d1 d2))
