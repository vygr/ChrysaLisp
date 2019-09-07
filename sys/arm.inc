;;;;;;;;;;;;;;;;;;;;
; ARM Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defmacro emit-ux (x s d)
	`(emit-int (+ 0xd3400000 (<< ,x 10) (<< (reg ,s) 5) (reg ,d))))

(defmacro emit-bf (o x y s d)
	`(emit-int (+ ,o (<< ,x 16) (<< ,y 10) (<< (reg ,s) 5) (reg ,d))))

(defmacro emit-rrr (o m n d)
	`(emit-int (+ ,o (<< (reg ,m) 16) (<< (reg ,n) 5) (reg ,d))))

(defmacro emit-ri (w o s d c)
	`(cond
		((<= -0x100 ,c 0xff)
			(emit-int (+ ,o (<< (logand ,c 0x1ff) 12) (<< (reg ,d) 5) (reg ,s))))
		((<= 0 ,c (<< 0xfff ,w))
			(emit-int (+ 0x1000000 ,o (<< ,c (- 10 ,w)) (<< (reg ,d) 5) (reg ,s))))
		(t (throw "emit-ri/ir constant out of range !" ,c))))

(defmacro emit-ir (w o s c d)
	`(emit-ri ,w ,o ,d ,s ,c))

(defmacro emit-iru (w o s c d)
	`(progn
		(unless (eql ,s ,d) (emit-xor-rr ,d ,d))
		(emit-ir ,w ,o ,s ,c ,d)
		(if (eql ,s ,d) (emit-ux ,(elem w '(7 15 31)) ,d ,d))))

(defmacro emit-rd (o s d1 d2)
	`(emit-int (+ ,o (<< (reg ,d2) 16) (<< (reg ,d1) 5) (reg ,s))))

(defmacro emit-dr (o s1 s2 d)
	`(emit-int (+ ,o (<< (reg ,s2) 16) (<< (reg ,s1) 5) (reg ,d))))

(defmacro emit-dru (o x s1 s2 d)
	`(progn
		(and (not (eql ,s1 ,d)) (not (eql ,s2 ,d)) (emit-xor-rr ,d ,d))
		(emit-dr ,o ,s1 ,s2 ,d)
		(if (or (eql ,s1 ,d) (eql ,s2 ,d)) (emit-ux ,x ,d ,d))))

(defmacro emit-branch (o l)
	`(cond
		((<= -0x100000 (defq ,(defq c (gensym)) (- ,l *pc*)) 0xffffc)
			(emit-int (+ 0x54000000 (<< (logand ,c 0x1ffffc) 3) ,o)))
		(t (throw "emit-branch constant out of range !" ,c))))

(defmacro emit-is-mask (_)
	`(and (/= 0 ,_) (= 0 (logand ,_ (inc ,_)))))

(defmacro emit-is-shifted-mask (_)
	`(and (/= 0 ,_) (emit-is-mask (logior ,_ (dec ,_)))))

(defcfun emit-limm (_)
	(if (or (= 0 _) (= -1 _)) -1
		(progn
			(defq s 64 e t)
			(while e
				(defq s (>> s 1) m (dec (<< 1 s)))
				(if (/= (logand _ m) (logand (>> _ s) m))
					(setq s (* s 2) e nil)
					(if (<= s 2)
						(setq e nil))))
			(defq m (>> -1 (- 64 s)) _ (logand _ m))
			(if (emit-is-shifted-mask _)
				(progn
					(defq i (ntz _))
					(if (>= i 64)
						(setq e t)
						(defq cto (nto (>> _ i)))))
				(progn
					(setq _ (logior _ (lognot m)))
					(if (not (emit-is-shifted-mask (lognot _)))
						(setq e t)
						(defq clo (nlo _) i (- 64 clo)
							cto (- (+ clo (nto _)) (- 64 s))))))
			(if (or e (<= s i)) -1
				(progn
					(defq _r (logand (- s i) (dec s))
						_s (<< (lognot (dec s)) 1)
						_s (logior _s (dec cto))
						n (logxor (logand (>> _s 6) 1) 1))
					(+ (<< n 12) (<< _r 6) (logand _s 0x3f)))))))

(defcfun-bind reg (r)
	(find r '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
		r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 rsp)))

(each (lambda (_) (defcvar _ _)) '(r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30))
(defcvar 'stack_align 16 'stack_state '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r30))

(defcfun stack-init ()
	(defq tk_state_size (* ptr_size (length stack_state)))
	(vp-and-cr (neg stack_align) r1)
	(vp-sub-cr tk_state_size r1)
	(f-bind 'sys_task 'stop r2)
	(vp-cpy-ri r4 r1 (- tk_state_size ptr_size))
	(vp-cpy-ri r2 r1 (- tk_state_size (* ptr_size 2)))
	(vp-lea-p 'start r4)
	(vp-cpy-ri r4 r1 0)
	(goto 'funcexit)
(vp-label 'start)
	(vp-cpy-rr r1 r30)
	(vp-jmp-r r0)
(vp-label 'funcexit))

(defcfun-bind emit-push (&rest b)
	(cond
		((= 0 (defq l (length b))))
		((= 1 l)
			(emit-int (+ 0xf81f0c00 (<< (reg rsp) 5) (reg (elem 0 b)))))
		((= 2 l)
			(emit-int (+ 0xa9bf0000
				(<< (reg (elem 0 b)) 10) (<< (reg rsp) 5) (reg (elem 1 b)))))
		(t
			(emit-sub-cr (align (* ptr_size l) stack_align) rsp)
			(defq _ l)
			(while (>= (setq _ (- _ 2)) 0)
				(emit-int (+ 0xa9000000 (<< (- l _ 2) 15)
					(<< (reg (elem _ b)) 10) (<< (reg rsp) 5) (reg (elem (inc _) b)))))
			(if (/= -2 _) (emit-cpy-ri (elem 0 b) rsp (* ptr_size (dec l)))))))

(defcfun-bind emit-pop (&rest b)
	(cond
		((= 0 (defq l (length b))))
		((= 1 l)
			(emit-int (+ 0xf8410400 (<< (reg rsp) 5) (reg (elem 0 b)))))
		((= 2 l)
			(emit-int (+ 0xa8c10000
				(<< (reg (elem 0 b)) 10) (<< (reg rsp) 5) (reg (elem 1 b)))))
		(t
			(defq _ l)
			(while (>= (setq _ (- _ 2)) 0)
				(emit-int (+ 0xa9400000 (<< (- l _ 2) 15)
					(<< (reg (elem _ b)) 10) (<< (reg rsp) 5) (reg (elem (inc _) b)))))
			(if (/= -2 _) (emit-cpy-ir rsp (* ptr_size (dec l)) (elem 0 b)))
			(emit-add-cr (align (* ptr_size l) stack_align) rsp))))

(defcfun-bind emit-beq (l d) (emit-branch 0x0 l))
(defcfun-bind emit-bne (l d) (emit-branch 0x1 l))
(defcfun-bind emit-bge (l d) (emit-branch 0xa l))
(defcfun emit-blt (l d) (emit-branch 0xb l))
(defcfun emit-bgt (l d) (emit-branch 0xc l))
(defcfun emit-ble (l d) (emit-branch 0xd l))

(defcfun emit-seq-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x54000060) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sne-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x54000061) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-slt-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006b) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sle-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006d) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sgt-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006c) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sge-cr (c d)
	(emit-cmp-cr c d) (emit-int 0x5400006a) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))

(defcfun emit-seq-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x54000060) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sne-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x54000061) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-slt-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006b) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sle-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006d) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sgt-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006c) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))
(defcfun emit-sge-rr (s d)
	(emit-cmp-rr s d) (emit-int 0x5400006a) (emit-xor-rr d d) (emit-int 0x14000002) (emit-or-cr -1 d))

(defcfun emit-call (l)
	(emit-push r30)
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emit-int (+ 0x94000000 (>> (logand c 0xfffffff) 2))))
		(t (throw "emit-call constant out of range !" c)))
	(emit-pop r30))

(defcfun emit-call-r (r)
	(emit-push r30)
	(emit-int (+ 0xd63f0000 (<< (reg r) 5)))
	(emit-pop r30))

(defcfun emit-call-i (d c)
	(emit-cpy-ir d c r29)
	(emit-call-r r29))

(defcfun-bind emit-call-p (l)
	(emit-cpy-pr l r29)
	(emit-call-r r29))

(defcfun-bind emit-jmp (l d)
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emit-int (+ 0x14000000 (>> (logand c 0xfffffff) 2))))
		(t (throw "emit-jmp constant out of range !" c))))

(defcfun emit-jmp-r (r)
	(emit-int (+ 0xd61f0000 (<< (reg r) 5))))

(defcfun emit-jmp-i (d c)
	(emit-cpy-ir d c r29)
	(emit-jmp-r r29))

(defcfun emit-jmp-p (l)
	(emit-cpy-pr l r29)
	(emit-jmp-r r29))

(defcfun emit-lea-i (s c d)
	(cond
		((and (= c 0) (eql s d)))
		((<= -0x1000 c 0xfff)
			(if (>= c 0)
				(emit-int (+ 0x91000000 (<< (logand c 0xfff) 10) (<< (reg s) 5) (reg d)))
				(emit-int (+ 0xd1000000 (<< (logand (neg c) 0xfff) 10) (<< (reg s) 5) (reg d)))))
		(t (throw "emit-lea-i constant out of range !" c))))

(defcfun emit-lea-d (s1 s2 d) (emit-rrr 0x8b000000 s1 s2 d))

(defcfun-bind emit-lea-p (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(emit-int (+ 0x10000000 (<< (logand c 0x3) 29) (<< (logand c 0x1ffffc) 3) (reg d))))
		(t (throw "emit-lea-p constant out of range !" c))))

(defcfun emit-cpy-ir (s c d)
	(if (eql d rsp)
		(progn
			(emit-ir 3 0xf8400000 s c r29)
			(emit-cpy-rr r29 rsp))
		(emit-ir 3 0xf8400000 s c d)))

(defcfun emit-cpy-ir-b (s c d) (emit-ir 0 0x38800000 s c d))
(defcfun emit-cpy-ir-s (s c d) (emit-ir 1 0x78800000 s c d))
(defcfun-bind emit-cpy-ir-i (s c d) (emit-ir 2 0xb8800000 s c d))

(defcfun emit-cpy-ri (s d c)
	(when (eql s rsp)
		(emit-cpy-rr rsp r29)
		(setq s r29))
	(emit-ri 3 0xf8000000 s d c))

(defcfun emit-cpy-ri-b (s d c) (emit-ri 0 0x38000000 s d c))
(defcfun emit-cpy-ri-s (s d c) (emit-ri 1 0x78000000 s d c))
(defcfun-bind emit-cpy-ri-i (s d c) (emit-ri 2 0xb8000000 s d c))

(defcfun emit-cpy-ir-ub (s c d) (emit-iru 0 0x38400000 s c d))
(defcfun emit-cpy-ir-us (s c d) (emit-iru 1 0x78400000 s c d))
(defcfun-bind emit-cpy-ir-ui (s c d) (emit-iru 2 0xb8400000 s c d))

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

(defcfun-bind emit-cpy-pr (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xffffc)
			(emit-int (+ 0x58000000 (<< (logand c 0x1fffff) 3) (reg d))))
		(t (throw "emit-cpy-pr constant out of range !" c))))

(defcfun emit-mov-cr (c r)
	(defq r (reg r) x (logand c 0xffff00000000) y (logand c 0xffff0000) z (logand c 0xffff))
	(if (< c 0)
		(cond
			((>= c -0x10000)
				(emit-int (+ 0x92800000 (<< (lognot c) 5) r)))
			((>= c -0x100000000)
				(emit-int (+ 0x92a00000 (>> (logand (lognot c) 0xffff0000) 11) r))
				(if (/= z 0xffff) (emit-int (+ 0xf2800000 (<< z 5) r))))
			((>= c -0x1000000000000)
				(emit-int (+ 0x92c00000 (>> (logand (lognot c) 0xffff00000000) 27) r))
				(if (/= y 0xffff0000) (emit-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0xffff) (emit-int (+ 0xf2800000 (<< z 5) r))))
			(t
				(emit-int (+ 0x92e00000 (>> (logand (lognot c) 0xffff000000000000) 43) r))
				(if (/= x 0xffff00000000) (emit-int (+ 0xf2c00000 (>> x 27) r)))
				(if (/= y 0xffff0000) (emit-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0xffff) (emit-int (+ 0xf2800000 (<< z 5) r)))))
		(cond
			((<= c 0xffff)
				(emit-int (+ 0xd2800000 (<< c 5) r)))
			((<= c 0xffffffff)
				(emit-int (+ 0xd2a00000 (>> (logand c 0xffff0000) 11) r))
				(if (/= z 0) (emit-int (+ 0xf2800000 (<< z 5) r))))
			((<= c 0xffffffffffff)
				(emit-int (+ 0xd2c00000 (>> (logand c 0xffff00000000) 27) r))
				(if (/= y 0) (emit-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0) (emit-int (+ 0xf2800000 (<< z 5) r))))
			(t
				(emit-int (+ 0xd2e00000 (>> (logand c 0xffff000000000000) 43) r))
				(if (/= x 0) (emit-int (+ 0xf2c00000 (>> x 27) r)))
				(if (/= y 0) (emit-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0) (emit-int (+ 0xf2800000 (<< z 5) r)))))))

(defcfun-bind emit-cpy-cr (c r)
	(if (/= -1 (defq i (emit-limm c)))
		(emit-int (+ 0xb2000000 (<< i 10) (<< (reg rsp) 5) (reg r)))
		(emit-mov-cr c r)))

(defcfun-bind emit-add-cr (c r)
	(cond
		((= c 0))
		((< c 0)
			(emit-sub-cr (neg c) r))
		((<= c 0xffffff)
			(defq f (logand c 0xfff) c (logand c 0xfff000))
			(if (/= 0 f)
				(emit-int (+ 0x91000000 (<< f 10) (<< (reg r) 5) (reg r))))
			(if (/= 0 c)
				(emit-int (+ 0x91400000 (>> c 2) (<< (reg r) 5) (reg r)))))
		(t (throw "emit-add-cr constant out of range !" c))))

(defcfun-bind emit-sub-cr (c r)
	(cond
		((= c 0))
		((< c 0)
			(emit-add-cr (neg c) r))
		((<= c 0xffffff)
			(defq f (logand c 0xfff) c (logand c 0xfff000))
			(if (/= 0 f)
				(emit-int (+ 0xd1000000 (<< f 10) (<< (reg r) 5) (reg r))))
			(if (/= 0 c)
				(emit-int (+ 0xd1400000 (>> c 2) (<< (reg r) 5) (reg r)))))
		(t (throw "emit-sub-cr constant out of range !" c))))

(defcfun emit-cmp-cr (c r)
	(cond
		((<= 0 c 0xfff)
			(emit-int (+ 0xf100001f (<< c 10) (<< (reg r) 5))))
		((<= -0xfff c 0)
			(emit-int (+ 0xb100001f (<< (neg c) 10) (<< (reg r) 5))))
		(t (emit-cpy-cr c r29)
			(emit-cmp-rr r29 r))))

(defcfun-bind emit-and-cr (c r)
	(cond
		((= c -1))
		((= c 0)
			(emit-xor-rr r r))
		((eql r rsp)
			(if (/= -1 (defq i (emit-limm c)))
				(progn
					(emit-cpy-rr rsp r29)
					(emit-int (+ 0x92000000 (<< i 10) (<< (reg r29) 5) (reg r))))
				(progn
					(emit-mov-cr c r29)
					(emit-and-rr r29 r))))
		(t (defq i (emit-limm c))
			(if (/= i -1)
				(emit-int (+ 0x92000000 (<< i 10) (<< (reg r) 5) (reg r)))
				(progn
					(emit-mov-cr c r29)
					(emit-and-rr r29 r))))))

(defcfun-bind emit-or-cr (c r)
	(cond
		((= c 0))
		((= c -1)
			(emit-cpy-cr -1 r))
		(t (defq i (emit-limm c))
			(if (/= i -1)
				(emit-int (+ 0xb2000000 (<< i 10) (<< (reg r) 5) (reg r)))
				(progn
					(emit-mov-cr c r29)
					(emit-or-rr r29 r))))))

(defcfun emit-xor-cr (c r)
	(cond
		((= c 0))
		(t (defq i (emit-limm c))
			(if (/= i -1)
				(emit-int (+ 0xd2000000 (<< i 10) (<< (reg r) 5) (reg r)))
				(progn
					(emit-mov-cr c r29)
					(emit-or-rr r29 r))))))

(defcfun emit-cpy-rr (s d)
	(cond
		((eql s d))
		((or (eql s rsp) (eql d rsp))
			(emit-int (+ 0x91000000 (<< (reg s) 5) (reg d))))
		(t (emit-int (+ 0xaa0003e0 (<< (reg s) 16) (reg d))))))

(defcfun-bind emit-add-rr (s d) (if (eql s rsp) (emit-rrr 0x8b206000 d s d) (emit-rrr 0x8b206000 s d d)))

(defcfun-bind emit-sub-rr (s d)
	(cond
		((eql s rsp)
			(throw "emit-sub-rr src can't be rsp !" (list s d)))
		(t (emit-rrr 0xcb206000 s d d))))

(defcfun emit-cmp-rr (s d)
	(cond
		((eql s rsp)
			(throw "emit-cmp-rr src can't be rsp !" (list s d)))
		(t (emit-rrr 0xeb206000 s d rsp))))

(defcfun emit-and-rr (s d)
	(cond
		((eql s d))
		((or (eql s rsp) (eql d rsp))
			(throw "emit-and-rr src/dst can't be rsp !" (list s d)))
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

(defcfun emit-lnot-rr (r d)
	(emit-cmp-cr 0 d)
	(emit-int (+ 0x9a9f17e0 (reg d))))

(defcfun emit-land-rr (s d)
	(emit-cmp-cr 0 d)
	(emit-int (+ 0xfa401804 (<< (reg s) 5)))
	(emit-int (+ 0x9a9f07e0 (reg d))))

(defmacro emit-shift-cr (e o c x y r)
	`(cond
		((= ,c 0))
		((eql ,r rsp)
			(throw ,(cat "emit-" e "-cr dst can't be rsp !") (list ,c ,r)))
		(t (emit-bf ,o ,x ,y ,r ,r))))

(defcfun-bind emit-shl-cr (c r) (emit-shift-cr "shl" 0xd3400000 c (logand (- 64 c) 63) (- 63 c) r))
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
		((= c 0)
			(emit-xor-rr r r))
		((= c -1)
			(emit-int (+ 0xcb000000 (<< (reg r) 16) (<< 31 5) (reg r))))
		(t
			(emit-cpy-cr c r29)
			(emit-mul-rr r29 r))))

(defcfun-bind emit-mul-rr (s d)
	(if (or (eql s rsp) (eql d rsp))
		(throw "emit-mul-rr src/dst can't be rsp !" (list s d)))
	(emit-int (+ 0x9b000000 (<< (reg d) 16) (<< 31 10) (<< (reg s) 5) (reg d))))

(defcfun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))

(defcfun emit-div-rrr (s d1 d2)
	(emit-int (+ 0x9ac00c00 (<< (reg s) 16) (<< (reg d2) 5) (reg r29)))
	(emit-int (+ 0x9b008000 (<< (reg r29) 16) (<< (reg d2) 10) (<< (reg s) 5) (reg d1)))
	(emit-cpy-rr r29 d2))

(defcfun emit-div-rrr-u (s d1 d2)
	(emit-int (+ 0x9ac00800 (<< (reg s) 16) (<< (reg d2) 5) (reg r29)))
	(emit-int (+ 0x9b008000 (<< (reg r29) 16) (<< (reg d2) 10) (<< (reg s) 5) (reg d1)))
	(emit-cpy-rr r29 d2))

(defcfun-bind emit-alloc (c) (emit-sub-cr (align c stack_align) rsp))
(defcfun-bind emit-free (c) (emit-add-cr (align c stack_align) rsp))
(defcfun-bind emit-ret () (emit-int 0xd65f03c0))
