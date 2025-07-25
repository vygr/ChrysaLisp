;;;;;;;;;;;;;;;;;;;;;;
; ARM64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defcvar 'stack_align 8 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r30))
(each (# (defcvar %0 %0)) '(:r15 :r16 :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30))

(defun emit-native-reg? (r)
	(find r '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7           ;args (x0-x7)
		:r15                                            ;indirect (x8)
		:r16 :r17 :r18 :r19 :r20 :r21 :r22              ;temps (x9-x15)
		:r23 :r24 :r25                                  ;ip0, ip1, platform
		:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r26 :r27 :rsp ;callee saves (x19-x28)
		:r29 :r30 :r31)))                               ;frm, lnk, stack

;VP stack is NOT the ARM64 stack !
;due to not wanting to suffer the 16 byte alignment restriction on calls etc.
(defq +arm64_rlnk (emit-native-reg? :r30)
	+arm64_rtmp (emit-native-reg? :r29)
	+arm64_vp_rsp (emit-native-reg? :rsp)
	+arm64_rsp (emit-native-reg? :r31))

(defmacro arm64-rrr (o m n d)
	`(emitm-int (+ ,o (<< ,m 16) (<< ,n 5) ,d)))

(defmacro arm64-ux (x s d)
	`(emitm-int (+ 0xd3400000 (<< ,x 10) (<< ,s 5) ,d)))

(defmacro arm64-bf (o x y s d)
	`(emitm-int (+ ,o (<< ,x 16) (<< ,y 10) (<< ,s 5) ,d)))

(defmacro arm64-ri (w o s d c)
	`(cond
		((<= -0x100 ,c 0xff)
			(emitm-int (+ ,o (<< (logand ,c 0x1ff) 12) (<< ,d 5) ,s)))
		((<= 0 ,c (<< 0xfff ,w))
			(emitm-int (+ 0x1000000 ,o (<< ,c (- 10 ,w)) (<< ,d 5) ,s)))
		(:t (throw "arm64-ri/ir constant out of range !" ,c))))

(defmacro arm64-ir (w o s c d)
	`(arm64-ri ,w ,o ,d ,s ,c))

(defun arm64-mov-cr (c r)
	(defq x (logand c 0xffff00000000) y (logand c 0xffff0000) z (logand c 0xffff))
	(if (< c 0)
		(cond
			((>= c -0x10000)
				(emitm-int (+ 0x92800000 (<< (lognot c) 5) r)))
			((>= c -0x100000000)
				(emitm-int (+ 0x92a00000 (>> (logand (lognot c) 0xffff0000) 11) r))
				(if (/= z 0xffff) (emitm-int (+ 0xf2800000 (<< z 5) r))))
			((>= c -0x1000000000000)
				(emitm-int (+ 0x92c00000 (>> (logand (lognot c) 0xffff00000000) 27) r))
				(if (/= y 0xffff0000) (emitm-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0xffff) (emitm-int (+ 0xf2800000 (<< z 5) r))))
			(:t (emitm-int (+ 0x92e00000 (>> (logand (lognot c) 0xffff000000000000) 43) r))
				(if (/= x 0xffff00000000) (emitm-int (+ 0xf2c00000 (>> x 27) r)))
				(if (/= y 0xffff0000) (emitm-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0xffff) (emitm-int (+ 0xf2800000 (<< z 5) r)))))
		(cond
			((<= c 0xffff)
				(emitm-int (+ 0xd2800000 (<< c 5) r)))
			((<= c 0xffffffff)
				(emitm-int (+ 0xd2a00000 (>> (logand c 0xffff0000) 11) r))
				(if (/= z 0) (emitm-int (+ 0xf2800000 (<< z 5) r))))
			((<= c 0xffffffffffff)
				(emitm-int (+ 0xd2c00000 (>> (logand c 0xffff00000000) 27) r))
				(if (/= y 0) (emitm-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0) (emitm-int (+ 0xf2800000 (<< z 5) r))))
			(:t (emitm-int (+ 0xd2e00000 (>> (logand c 0xffff000000000000) 43) r))
				(if (/= x 0) (emitm-int (+ 0xf2c00000 (>> x 27) r)))
				(if (/= y 0) (emitm-int (+ 0xf2a00000 (>> y 11) r)))
				(if (/= z 0) (emitm-int (+ 0xf2800000 (<< z 5) r)))))))

(defmacro arm64-is-mask (_)
	`(and (/= 0 ,_) (= 0 (logand ,_ (inc ,_)))))

(defmacro arm64-is-shifted-mask (_)
	`(and (/= 0 ,_) (arm64-is-mask (logior ,_ (dec ,_)))))

(defun arm64-limm (_)
	(if (or (= 0 _) (= -1 _)) -1
		(progn
			(defq s 64 e :t)
			(while e
				(defq s (>> s 1) m (dec (<< 1 s)))
				(if (/= (logand _ m) (logand (>> _ s) m))
					(setq s (* s 2) e :nil)
					(if (<= s 2)
						(setq e :nil))))
			(defq m (>> -1 (- 64 s)) _ (logand _ m))
			(if (arm64-is-shifted-mask _)
				(progn
					(defq i (ntz _))
					(if (>= i 64)
						(setq e :t)
						(defq cto (nto (>> _ i)))))
				(progn
					(setq _ (logior _ (lognot m)))
					(if (not (arm64-is-shifted-mask (lognot _)))
						(setq e :t)
						(defq clo (nlo _) i (- 64 clo)
							cto (- (+ clo (nto _)) (- 64 s))))))
			(if (or e (<= s i)) -1
				(progn
					(defq _r (logand (- s i) (dec s))
						_s (<< (lognot (dec s)) 1)
						_s (logior _s (dec cto))
						n (logxor (logand (>> _s 6) 1) 1))
					(+ (<< n 12) (<< _r 6) (logand _s 0x3f)))))))

(defun emit-cpy-rr (s d)
	(cond
		((eql s d))
		((or (eql s +arm64_rsp) (eql d +arm64_rsp))
			(emitm-int (+ 0x91000000 (<< s 5) d)))
		(:t (emitm-int (+ 0xaa0003e0 (<< s 16) d)))))

(defun emit-xor-rr (s d)
	(cond
		((or (eql s +arm64_rsp) (eql d +arm64_rsp))
			(throw "emit-xor-rr src/dst can't be :rsp !" (list s d)))
		(:t (arm64-rrr 0xca000000 s d d))))

(defun emit-and-rr (s d)
	(cond
		((eql s d))
		((or (eql s +arm64_rsp) (eql d +arm64_rsp))
			(throw "emit-and-rr src/dst can't be :rsp !" (list s d)))
		(:t (arm64-rrr 0x8a000000 s d d))))

(defun arm64-cmp-rr (s d)
	(cond
		((eql s +arm64_rsp)
			(throw "arm64-cmp-rr src can't be :rsp !" (list s d)))
		(:t (arm64-rrr 0xeb206000 s d +arm64_rsp))))

(defun emit-cpy-cr (c r)
	(if (/= -1 (defq i (arm64-limm c)))
		(emitm-int (+ 0xb2000000 (<< i 10) (const (<< +arm64_rsp 5)) r))
		(arm64-mov-cr c r)))

(defun arm64-cmp-cr (c r)
	(cond
		((<= 0 c 0xfff)
			(emitm-int (+ 0xf100001f (<< c 10) (<< r 5))))
		((<= -0xfff c 0)
			(emitm-int (+ 0xb100001f (<< (neg c) 10) (<< r 5))))
		(:t (emit-cpy-cr c +arm64_rtmp)
			(arm64-cmp-rr +arm64_rtmp r))))

(defun emit-cpy-ir (s c d)
	(if (eql d +arm64_rsp)
		(progn
			(arm64-ir 3 0xf8400000 s c +arm64_rtmp)
			(emit-cpy-rr +arm64_rtmp +arm64_rsp))
		(arm64-ir 3 0xf8400000 s c d)))

(defmacro arm64-iru (w o s c d)
	`(progn
		(unless (eql ,s ,d) (emit-xor-rr ,d ,d))
		(arm64-ir ,w ,o ,s ,c ,d)
		(if (eql ,s ,d) (arm64-ux ,(elem-get '(7 15 31) w) ,d ,d))))

(defmacro arm64-rd (o s d1 d2)
	`(emitm-int (+ ,o (<< ,d2 16) (<< ,d1 5) ,s)))

(defmacro arm64-dr (o s1 s2 d)
	`(emitm-int (+ ,o (<< ,s2 16) (<< ,s1 5) ,d)))

(defmacro arm64-dru (o x s1 s2 d)
	`(progn
		(and (not (eql ,s1 ,d)) (not (eql ,s2 ,d)) (emit-xor-rr ,d ,d))
		(arm64-dr ,o ,s1 ,s2 ,d)
		(if (or (eql ,s1 ,d) (eql ,s2 ,d)) (arm64-ux ,x ,d ,d))))

(defmacro arm64-branch (o l)
	`(cond
		((<= -0x100000 (defq ,(defq c (gensym)) (- ,l *pc*)) 0xffffc)
			(emitm-int (+ 0x54000000 (<< (logand ,c 0x1ffffc) 3) ,o)))
		(:t (throw "arm64-branch constant out of range !" ,c))))

(defun emit-add-cr (c r)
	(cond
		((= c 0))
		((< c 0)
			;sub-cr
			(cond
				((<= (setq c (neg c)) 0xffffff)
					(defq f (logand c 0xfff) c (logand c 0xfff000))
					(if (/= 0 f)
						(emitm-int (+ 0xd1000000 (<< f 10) (<< r 5) r)))
					(if (/= 0 c)
						(emitm-int (+ 0xd1400000 (>> c 2) (<< r 5) r))))
				(:t (throw "emit-add-cr constant out of range !" c))))
		((<= c 0xffffff)
			(defq f (logand c 0xfff) c (logand c 0xfff000))
			(if (/= 0 f)
				(emitm-int (+ 0x91000000 (<< f 10) (<< r 5) r)))
			(if (/= 0 c)
				(emitm-int (+ 0x91400000 (>> c 2) (<< r 5) r))))
		(:t (throw "emit-add-cr constant out of range !" c))))

(defun emit-sub-cr (c r)
	(cond
		((= c 0))
		((< c 0)
			;add-cr
			(cond
				((<= (setq c (neg c)) 0xffffff)
					(defq f (logand c 0xfff) c (logand c 0xfff000))
					(if (/= 0 f)
						(emitm-int (+ 0x91000000 (<< f 10) (<< r 5) r)))
					(if (/= 0 c)
						(emitm-int (+ 0x91400000 (>> c 2) (<< r 5) r))))
				(:t (throw "emit-sub-cr constant out of range !" c))))
		((<= c 0xffffff)
			(defq f (logand c 0xfff) c (logand c 0xfff000))
			(if (/= 0 f)
				(emitm-int (+ 0xd1000000 (<< f 10) (<< r 5) r)))
			(if (/= 0 c)
				(emitm-int (+ 0xd1400000 (>> c 2) (<< r 5) r))))
		(:t (throw "emit-sub-cr constant out of range !" c))))

(defun emit-or-rr (s d)
	(cond
		((eql s d))
		((or (eql s +arm64_rsp) (eql d +arm64_rsp))
			(throw "emit-or-rr src/dst can't be :rsp !" (list s d)))
		(:t (arm64-rrr 0xaa000000 s d d))))

(defun emit-or-cr (c r)
	(cond
		((= c 0))
		((= c -1)
			(emit-cpy-cr -1 r))
		(:t (defq i (arm64-limm c))
			(if (/= i -1)
				(emitm-int (+ 0xb2000000 (<< i 10) (<< r 5) r))
				(progn
					(arm64-mov-cr c +arm64_rtmp)
					(emit-or-rr +arm64_rtmp r))))))

(defun emit-cpy-ri (s d c)
	(when (eql s +arm64_rsp)
		(emit-cpy-rr +arm64_rsp +arm64_rtmp)
		(setq s +arm64_rtmp))
	(arm64-ri 3 0xf8000000 s d c))

(defun emit-push (&rest b)
	(each (# (if (= (length %0) 2)
		;pairs, first allocating the space
		(if (= (!) 0)
			(emitm-int (+ 0xa9800000
				(<< (logand (neg (align (* +ptr_size (length b)) stack_align)) 0x3f8) 12)
				(<< (second %0) 10) (const (<< +arm64_vp_rsp 5)) (first %0)))
			(emitm-int (+ 0xa9000000 (<< (* (!) 2) 15)
				(<< (second %0) 10) (const (<< +arm64_vp_rsp 5)) (first %0))))
		;singles, first allocating the space
		(if (= (!) 0)
			(emitm-int (+ 0xf8000c00
				(<< (logand (neg (align (* +ptr_size (length b)) stack_align)) 0x1ff) 12)
				(const (<< +arm64_vp_rsp 5)) (first %0)))
			(emit-cpy-ri (first %0) +arm64_vp_rsp (* +ptr_size (* (!) 2))))))
		(partition (reverse b) 2)))

(defun emit-pop (&rest b)
	(reach (# (if (= (length %0) 2)
		;pairs, last freeing the space
		(if (= (!) 0)
			(emitm-int (+ 0xa8c00000
				(<< (logand (align (* +ptr_size (length b)) stack_align) 0x3f8) 12)
				(<< (second %0) 10) (const (<< +arm64_vp_rsp 5)) (first %0)))
			(emitm-int (+ 0xa9400000 (<< (* (!) 2) 15)
				(<< (second %0) 10) (const (<< +arm64_vp_rsp 5)) (first %0))))
		;singles, last freeing the space
		(if (= (!) 0)
			(emitm-int (+ 0xf8400400
				(<< (logand (align (* +ptr_size (length b)) stack_align) 0x1ff) 12)
				(const (<< +arm64_vp_rsp 5)) (first %0)))
			(emit-cpy-ir +arm64_vp_rsp (* +ptr_size (* (!) 2)) (first %0)))))
		(partition (reverse b) 2)))

(defun emit-beq-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0x0 l))
(defun emit-bne-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0x1 l))
(defun emit-bge-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0xa l))
(defun emit-blt-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0xb l))
(defun emit-bgt-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0xc l))
(defun emit-ble-cr (c d l m) (arm64-cmp-cr c d) (arm64-branch 0xd l))

(defun emit-beq-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0x0 l))
(defun emit-bne-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0x1 l))
(defun emit-bge-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0xa l))
(defun emit-blt-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0xb l))
(defun emit-bgt-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0xc l))
(defun emit-ble-rr (s d l m) (arm64-cmp-rr s d) (arm64-branch 0xd l))

(defun emit-seq-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x54000060) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sne-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x54000061) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-slt-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x5400006b) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sle-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x5400006d) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sgt-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x5400006c) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sge-cr (c d)
	(arm64-cmp-cr c d) (emitm-int 0x5400006a) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))

(defun emit-seq-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x54000060) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sne-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x54000061) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-slt-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x5400006b) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sle-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x5400006d) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sgt-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x5400006c) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))
(defun emit-sge-rr (s d)
	(arm64-cmp-rr s d) (emitm-int 0x5400006a) (emit-xor-rr d d) (emitm-int 0x14000002) (emit-or-cr -1 d))

(defun emit-call (l)
	(emit-push +arm64_rlnk)
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emitm-int (+ 0x94000000 (>> (logand c 0xfffffff) 2))))
		(:t (throw "emit-call constant out of range !" c)))
	(emit-pop +arm64_rlnk))

(defun emit-cpy-pr (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xffffc)
			(emitm-int (+ 0x58000000 (<< (logand c 0x1fffff) 3) d)))
		(:t (throw "emit-cpy-pr constant out of range !" c))))

(defun emit-call-r (r)
	(emit-push +arm64_rlnk)
	(emitm-int (+ 0xd63f0000 (<< r 5)))
	(emit-pop +arm64_rlnk))

(defun emit-call-i (d c)
	(emit-cpy-ir d c +arm64_rtmp)
	(emit-call-r +arm64_rtmp))

(defun emit-call-p (l)
	(emit-cpy-pr l +arm64_rtmp)
	(emit-call-r +arm64_rtmp))

(defun emit-jmp (l d)
	(cond
		((<= -0x8000000 (defq c (- l *pc*)) 0x7fffffc)
			(emitm-int (+ 0x14000000 (>> (logand c 0xfffffff) 2))))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-jmp-r (r)
	(emitm-int (+ 0xd61f0000 (<< r 5))))

(defun emit-jmp-i (d c)
	(emit-cpy-ir d c +arm64_rtmp)
	(emit-jmp-r +arm64_rtmp))

(defun emit-jmp-p (l)
	(emit-cpy-pr l +arm64_rtmp)
	(emit-jmp-r +arm64_rtmp))

(defun emit-lea-i (s c d)
	(cond
		((and (= c 0) (eql s d)))
		((<= -0x1000 c 0xfff)
			(if (>= c 0)
				(emitm-int (+ 0x91000000 (<< (logand c 0xfff) 10) (<< s 5) d))
				(emitm-int (+ 0xd1000000 (<< (logand (neg c) 0xfff) 10) (<< s 5) d))))
		(:t (throw "emit-lea-i constant out of range !" c))))

(defun emit-lea-d (s1 s2 d) (arm64-rrr 0x8b000000 s1 s2 d))

(defun emit-lea-p (l d)
	(cond
		((<= -0x100000 (defq c (- l *pc*)) 0xfffff)
			(emitm-int (+ 0x10000000 (<< (logand c 0x3) 29) (<< (logand c 0x1ffffc) 3) d)))
		(:t (throw "emit-lea-p constant out of range !" c))))

(defun emit-cpy-ir-b (s c d) (arm64-ir 0 0x38800000 s c d))
(defun emit-cpy-ir-s (s c d) (arm64-ir 1 0x78800000 s c d))
(defun emit-cpy-ir-i (s c d) (arm64-ir 2 0xb8800000 s c d))

(defun emit-cpy-ri-b (s d c) (arm64-ri 0 0x38000000 s d c))
(defun emit-cpy-ri-s (s d c) (arm64-ri 1 0x78000000 s d c))
(defun emit-cpy-ri-i (s d c) (arm64-ri 2 0xb8000000 s d c))

(defun emit-cpy-ir-ub (s c d) (arm64-iru 0 0x38400000 s c d))
(defun emit-cpy-ir-us (s c d) (arm64-iru 1 0x78400000 s c d))
(defun emit-cpy-ir-ui (s c d) (arm64-ir 2 0xb8400000 s c d))

(defun emit-cpy-rd (s d1 d2) (arm64-rd 0xf8206800 s d2 d1))
(defun emit-cpy-rd-b (s d2 d1) (arm64-rd 0x38206800 s d2 d1))
(defun emit-cpy-rd-s (s d2 d1) (arm64-rd 0x78206800 s d2 d1))
(defun emit-cpy-rd-i (s d2 d1) (arm64-rd 0xb8206800 s d2 d1))

(defun emit-cpy-dr (s1 s2 d) (arm64-dr 0xf8606800 s2 s1 d))
(defun emit-cpy-dr-b (s1 s2 d) (arm64-dr 0x38a06800 s2 s1 d))
(defun emit-cpy-dr-s (s1 s2 d) (arm64-dr 0x78a06800 s2 s1 d))
(defun emit-cpy-dr-i (s1 s2 d) (arm64-dr 0xb8a06800 s2 s1 d))
(defun emit-cpy-dr-ub (s1 s2 d) (arm64-dru 0x38606800 7 s1 s2 d))
(defun emit-cpy-dr-us (s1 s2 d) (arm64-dru 0x78606800 15 s1 s2 d))
(defun emit-cpy-dr-ui (s1 s2 d) (arm64-dru 0xb8606800 31 s1 s2 d))

(defun emit-and-cr (c r)
	(cond
		((= c -1))
		((= c 0) (emit-xor-rr r r))
		((eql r +arm64_rsp)
			(if (/= -1 (defq i (arm64-limm c)))
				(progn
					(emit-cpy-rr +arm64_rsp +arm64_rtmp)
					(emitm-int (+ 0x92000000 (<< i 10) (<< +arm64_rtmp 5) r)))
				(progn
					(arm64-mov-cr c +arm64_rtmp)
					(emit-and-rr +arm64_rtmp r))))
		(:t (defq i (arm64-limm c))
			(if (/= i -1)
				(emitm-int (+ 0x92000000 (<< i 10) (<< r 5) r))
				(progn
					(arm64-mov-cr c +arm64_rtmp)
					(emit-and-rr +arm64_rtmp r))))))

(defun emit-xor-cr (c r)
	(cond
		((= c 0))
		(:t (defq i (arm64-limm c))
			(if (/= i -1)
				(emitm-int (+ 0xd2000000 (<< i 10) (<< r 5) r))
				(progn
					(arm64-mov-cr c +arm64_rtmp)
					(emit-xor-rr +arm64_rtmp r))))))

(defun emit-add-rr (s d)
	(if (eql s +arm64_rsp) (arm64-rrr 0x8b206000 d s d) (arm64-rrr 0x8b206000 s d d)))

(defun emit-sub-rr (s d)
	(cond
		((eql s +arm64_rsp)
			(throw "emit-sub-rr src can't be :rsp !" (list s d)))
		(:t (arm64-rrr 0xcb206000 s d d))))

(defun emit-lnot-rr (r d)
	(arm64-cmp-cr 0 d)
	(emitm-int (+ 0x9a9f17e0 d)))

(defun emit-land-rr (s d)
	(arm64-cmp-cr 0 d)
	(emitm-int (+ 0xfa401804 (<< s 5)))
	(emitm-int (+ 0x9a9f07e0 d)))

(defmacro arm64-shift-cr (e o c x y r)
	`(cond
		((= ,c 0))
		((eql ,r ,+arm64_rsp)
			(throw ,(cat "emit-" e "-cr dst can't be :rsp !") (list ,c ,r)))
		(:t (arm64-bf ,o ,x ,y ,r ,r))))

(defun emit-shl-cr (c r) (arm64-shift-cr "shl" 0xd3400000 c (logand (- 64 c) 63) (- 63 c) r))
(defun emit-shr-cr (c r) (arm64-shift-cr "shr" 0xd3400000 c c 63 r))
(defun emit-asr-cr (c r) (arm64-shift-cr "asr" 0x93400000 c c 63 r))

(defmacro arm64-shift-rr (e o s d)
	`(cond
		((or (eql ,s ,+arm64_rsp) (eql ,d ,+arm64_rsp))
			(throw ,(cat "emit-" e "-rr src/dst can't be :rsp !") (list ,s ,d)))
		(:t (arm64-rrr ,o ,s ,d ,d))))

(defun emit-shl-rr (s d) (arm64-shift-rr "shl" 0x9ac02000 s d))
(defun emit-shr-rr (s d) (arm64-shift-rr "shr" 0x9ac02400 s d))
(defun emit-asr-rr (s d) (arm64-shift-rr "asr" 0x9ac02800 s d))

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s +arm64_rtmp)
		(emit-cpy-rr d s)
		(emit-cpy-rr +arm64_rtmp d)))

(defun emit-mul-rr (s d)
	(if (or (eql s +arm64_rsp) (eql d +arm64_rsp))
		(throw "emit-mul-rr src/dst can't be :rsp !" (list s d)))
	(emitm-int (+ 0x9b000000 (<< d 16) (const (<< 31 10)) (<< s 5) d)))

(defun emit-mul-cr (c r)
	(cond
		((= c 1))
		((= c 0) (emit-xor-rr r r))
		((= c -1) (emitm-int (+ 0xcb000000 (<< r 16) (const (<< 31 5)) r)))
		((defq s (log2 c)) (emit-shl-cr s r))
		(:t (emit-cpy-cr c +arm64_rtmp)
			(emit-mul-rr +arm64_rtmp r))))

(defun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))

(defun emit-div-rrr (s d1 d2)
	(emitm-int (+ 0x9ac00c00 (<< s 16) (<< d2 5) +arm64_rtmp))
	(emitm-int (+ 0x9b008000 (const (<< +arm64_rtmp 16)) (<< d2 10) (<< s 5) d1))
	(emit-cpy-rr +arm64_rtmp d2))

(defun emit-div-rrr-u (s d1 d2)
	(emitm-int (+ 0x9ac00800 (<< s 16) (<< d2 5) +arm64_rtmp))
	(emitm-int (+ 0x9b008000 (const (<< +arm64_rtmp 16)) (<< d2 10) (<< s 5) d1))
	(emit-cpy-rr +arm64_rtmp d2))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) +arm64_vp_rsp))
(defun emit-free (c) (emit-add-cr (align c stack_align) +arm64_vp_rsp))
(defun emit-ret () (emitm-int 0xd65f03c0))
(defun emit-sync (n) (emitm-int 0xd5033fbf))
(defun emit-brk (n))

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-sub-cr tk_state_size s)
	(emit-and-cr -16 s)
	(emit-cpy-ri f s (- tk_state_size +ptr_size))
	(emit-cpy-ri x s (- tk_state_size (* +ptr_size 2)))
	(emit-lea-p (+ *pc* 12) f)
	(emit-cpy-ri f s 0)
	(emit-jmp (+ *pc* 12) 0)
;arm_start
	(emit-cpy-rr (const (emit-native-reg? :r1)) +arm64_rlnk)
	(emit-jmp-r (const (emit-native-reg? :r0))))
;arm_exit

(case *abi*
(ARM64
	(defun emit-call-abi (r b c n &rest x)
		(defq s (* +ptr_size (length x)))
		(cond
			((= (% *func_align* 16) 0)
				;push frame
				(emit-cpy-rr +arm64_vp_rsp +arm64_rsp)
				(emit-cpy-rr +arm64_rlnk +arm64_rtmp)
				(each (# (if (= (length %0) 2)
					;pairs, first allocating the space
					(if (= (!) 0)
						(emitm-int (+ 0xa9800000
							(<< (logand (neg (align s 16)) 0x3f8) 12)
							(<< (second %0) 10) (const (<< +arm64_rsp 5)) (first %0)))
						(emitm-int (+ 0xa9000000 (<< (* (!) 2) 15)
							(<< (second %0) 10) (const (<< +arm64_rsp 5)) (first %0))))
					;singles, first allocating the space
					(if (= (!) 0)
						(emitm-int (+ 0xf8000c00
							(<< (logand (neg (align s 16)) 0x1ff) 12)
							(const (<< +arm64_rsp 5)) (first %0)))
						(emit-cpy-ri (first %0) +arm64_rsp (* +ptr_size (* (!) 2))))))
					(partition (reverse x) 2))
				;call via abi table
				(emit-cpy-ir b c r)
				(emitm-int (+ 0xd63f0000 (<< r 5)))
				;pop frame
				(emit-cpy-rr +arm64_rtmp +arm64_rlnk))
			(:t (emit-lea-i +arm64_vp_rsp (neg s) +arm64_rsp)
				(emit-and-cr -16 +arm64_rsp)
				(emit-cpy-rr +arm64_rlnk +arm64_rtmp)
				(each (# (if (= (length %0) 2)
						(emitm-int (+ 0xa9000000 (<< (* (!) 2) 15)
							(<< (second %0) 10) (const (<< +arm64_rsp 5)) (first %0)))
						(emit-cpy-ri (first %0) +arm64_rsp (* +ptr_size (* (!) 2)))))
					(partition (reverse x) 2))
				(emit-cpy-ir b c r)
				(emitm-int (+ 0xd63f0000 (<< r 5)))
				(emit-cpy-rr +arm64_rtmp +arm64_rlnk)))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

;module, that exports everything !
(export-symbols (map (const first) (tolist (env))))
(env-pop)
