;;;;;;;;;;;;;;;;;;;;
; ARM Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defcfun reg (r)
	(find r '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
			_ _ _ _ _ _ _ _ _ _ _ _ _ _ r29 r30 rsp)))

(defcvar 'r29 'r29 'r30 'r30 'stack_align 16
	'stack_state '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r30))

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
	(if (ne 0 (bit-and 1 (length b)))
		(setq b (cat '(r29) b)))
	(emit-sub-cr (mul ptr_size (length b)) rsp)
	(defq _ -2)
	(while (ne (setq _ (add 2 _)) (length b))
		(emit-int (add 0xa9000000 (bit-shl (sub (length b) _ 1) 15)
			(bit-shl (reg (elem (inc _) b)) 10) (bit-shl (reg rsp) 5) (reg (elem _ b))))))

(defcfun emit-pop (&rest b)
	(if (ne 0 (bit-and 1 (length b)))
		(setq b (cat '(r29) b)))
	(defq _ -2)
	(while (ne (setq _ (add 2 _)) (length b))
		(emit-int (add 0xa9400000 (bit-shl (sub (length b) _ 1) 15)
			(bit-shl (reg (elem (inc _) b)) 10) (bit-shl (reg rsp) 5) (reg (elem _ b)))))
	(emit-add-cr (mul ptr_size (length b)) rsp))

(defcfun emit-branch (o c d)
	(cond
		((le -0x100000 c 0xffffc)
			(emit-int (add 0x54000000 (bit-shl (bit-and c 0x1ffffc) 3) o)))
		(t (throw "emit-branch constant out of range" c))))

(defcfun emit-beq (l d) (emit-branch 0x0 (sub l *pc*) d))
(defcfun emit-bne (l d) (emit-branch 0x1 (sub l *pc*) d))
(defcfun emit-bge (l d) (emit-branch 0xa (sub l *pc*) d))
(defcfun emit-blt (l d) (emit-branch 0xb (sub l *pc*) d))
(defcfun emit-bgt (l d) (emit-branch 0xc (sub l *pc*) d))
(defcfun emit-ble (l d) (emit-branch 0xd (sub l *pc*) d))

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

(defcfun emit-lea-d (s1 s2 d)
	(emit-int (add 0x8b000000 (bit-shl (reg s1) 10) (bit-shl (reg s2) 5) (reg d))))

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

(defcfun emit-cpy-rr (s d)
	(unless (eql s d)
		(emit-int (add 0x91000000 (bit-shl (reg s) 5) (reg d)))))

(defcfun emit-cpy-ir (s c d)
	(if (eql d rsp)
		(progn
			(cond
				((le -0x100 c 0xff)
					(emit-int (add 0xf8400000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg r29))))
				((and (le 0 c 0x7ff8) (eq (bit-and c 7) 0))
					(emit-int (add 0xf9400000 (bit-shl c 7) (bit-shl (reg s) 5) (reg r29))))
				(t (throw "emit-cpy-ir constant out of range" c)))
			(emit-cpy-rr r29 rsp))
		(cond
			((le -0x100 c 0xff)
				(emit-int (add 0xf8400000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
			((and (le 0 c 0x7ff8) (eq (bit-and c 7) 0))
				(emit-int (add 0xf9400000 (bit-shl c 7) (bit-shl (reg s) 5) (reg d))))
			(t (throw "emit-cpy-ir constant out of range" c)))))

(defcfun emit-cpy-dr (s1 s2 d)
	(emit-int (add 0xf8606800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d))))

(defcfun emit-cpy-pr (l d)
	(defq c (sub l *pc*))
	(cond
		((le -0x100000 c 0xffffc)
			(emit-int (add 0x58000000 (bit-shl (bit-and c 0x1fffff) 3) (reg d))))
		(t (throw "emit-cpy-pr constant out of range" c))))

(defcfun emit-cpy-ri (s d c)
	(when (eql s rsp)
		(emit-cpy-rr rsp r29)
		(setq s r29))
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0xf8000000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg d) 5) (reg s))))
		((and (le 0 c 0x7ff8) (eq (bit-and c 7) 0))
			(emit-int (add 0xf9000000 (bit-shl c 7) (bit-shl (reg d) 5) (reg s))))
		(t (throw "emit-cpy-ri constant out of range" c))))

(defcfun emit-cpy-rd (d s1 s2)
	(emit-int (add 0xf8206800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d))))

(defcfun emit-cpy-rp (d l)
	(emit-lea-p l r29)
	(emit-cpy-ri d r29 0))

(defcfun emit-cpy-ir-b (s c d)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x38800000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((le 0 c 0xfff)
			(emit-int (add 0x39800000 (bit-shl c 10) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-b constant out of range" c))))

(defcfun emit-cpy-dr-b (s1 s2 d)
	(emit-int (add 0x38a06800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d))))

(defcfun emit-cpy-ir-ub (s c d)
	(unless (eql s d)
		(emit-xor-rr d d))
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x38400000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((le 0 c 0xfff)
			(emit-int (add 0x39400000 (bit-shl c 10) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-ub constant out of range" c)))
	(if (eql s d)
		(emit-int (add 0xd3400000 (bit-shl 7 10) (bit-shl (reg s) 5) (reg d)))))

(defcfun emit-cpy-dr-ub (s1 s2 d)
	(and (not (eql s1 d)) (not (eql s2 d))
		(emit-xor-rr d d))
	(emit-int (add 0x38606800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d)))
	(if (or (eql s1 d) (eql s2 d))
		(emit-int (add 0xd3400000 (bit-shl 7 10) (bit-shl (reg d) 5) (reg d)))))

(defcfun emit-cpy-ir-s (s c d)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x78800000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((and (le 0 c 0x1ffe) (eq (bit-and c 1) 0))
			(emit-int (add 0x79800000 (bit-shl c 9) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-s constant out of range" c))))

(defcfun emit-cpy-dr-s (s1 s2 d)
	(emit-int (add 0x78a06800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d))))

(defcfun emit-cpy-ir-us (s c d)
	(unless (eql s d)
		(emit-xor-rr d d))
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x78400000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((and (le 0 c 0x1ffe) (eq (bit-and c 1) 0))
			(emit-int (add 0x79400000 (bit-shl c 9) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-us constant out of range" c)))
	(if (eql s d)
		(emit-int (add 0xd3400000 (bit-shl 15 10) (bit-shl (reg s) 5) (reg d)))))

(defcfun emit-cpy-dr-us (s1 s2 d)
	(and (not (eql s1 d)) (not (eql s2 d))
		(emit-xor-rr d d))
	(emit-int (add 0x78606800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d)))
	(if (or (eql s1 d) (eql s2 d))
		(emit-int (add 0xd3400000 (bit-shl 15 10) (bit-shl (reg d) 5) (reg d)))))

(defcfun emit-cpy-ir-i (s c d)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0xb8800000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((and (le 0 c 0x3ffc) (eq (bit-and c 3) 0))
			(emit-int (add 0xb9800000 (bit-shl c 8) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-i constant out of range" c))))

(defcfun emit-cpy-dr-i (s1 s2 d)
	(emit-int (add 0xb8a06800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d))))

(defcfun emit-cpy-ir-ui (s c d)
	(unless (eql s d)
		(emit-xor-rr d d))
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0xb8400000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg s) 5) (reg d))))
		((and (le 0 c 0x3ffc) (eq (bit-and c 3) 0))
			(emit-int (add 0xb9400000 (bit-shl c 8) (bit-shl (reg s) 5) (reg d))))
		(t (throw "emit-cpy-ir-ui constant out of range" c)))
	(if (eql s d)
		(emit-int (add 0xd3400000 (bit-shl 31 10) (bit-shl (reg s) 5) (reg d)))))

(defcfun emit-cpy-dr-ui (s1 s2 d)
	(and (not (eql s1 d)) (not (eql s2 d))
		(emit-xor-rr d d))
	(emit-int (add 0xb8606800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg d)))
	(if (or (eql s1 d) (eql s2 d))
		(emit-int (add 0xd3400000 (bit-shl 31 10) (bit-shl (reg d) 5) (reg d)))))

(defcfun emit-cpy-ri-b (s d c)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x38000000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg d) 5) (reg s))))
		((le 0 c 0xfff)
			(emit-int (add 0x39000000 (bit-shl c 10) (bit-shl (reg d) 5) (reg s))))
		(t (throw "emit-cpy-ri-b constant out of range" c))))

(defcfun emit-cpy-rd-b (r s1 s2)
	(emit-int (add 0x38206800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg r))))

(defcfun emit-cpy-ri-s (s d c)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0x78000000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg d) 5) (reg s))))
		((and (le 0 c 0x1ffe) (eq (bit-and c 1) 0))
			(emit-int (add 0x79000000 (bit-shl c 9) (bit-shl (reg d) 5) (reg s))))
		(t (throw "emit-cpy-ri-s constant out of range" c))))

(defcfun emit-cpy-rd-s (r s1 s2)
	(emit-int (add 0x78206800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg r))))

(defcfun emit-cpy-ri-i (s d c)
	(cond
		((le -0x100 c 0xff)
			(emit-int (add 0xb8000000 (bit-shl (bit-and c 0x1ff) 12) (bit-shl (reg d) 5) (reg s))))
		((and (le 0 c 0x3ffc) (eq (bit-and c 3) 0))
			(emit-int (add 0xb9000000 (bit-shl c 8) (bit-shl (reg d) 5) (reg s))))
		(t (throw "emit-cpy-ri-i constant out of range" c))))

(defcfun emit-cpy-rd-i (r s1 s2)
	(emit-int (add 0xb8206800 (bit-shl (reg s2) 16) (bit-shl (reg s1) 5) (reg r))))

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

(defcfun emit-add-rr (s d)
	(emit-int (add 0x8b000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

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

(defcfun emit-sub-rr (s d)
	(emit-int (add 0xcb000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

(defcfun emit-cmp-cr (c r)
	(emit-cpy-cr c r29)
	(emit-cmp-rr r29 r))

(defcfun emit-cmp-rr (s d)
	(emit-int (add 0xeb000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) 31)))

(defcfun emit-and-cr (c r)
	(unless (eq c -1)
		(if (eq c 0)
			(emit-xor-rr r r)
			(progn
				(emit-cpy-cr c r29)
				(emit-and-rr r29 r)))))

(defcfun emit-and-rr (s d)
	(unless (eql s d)
		(emit-int (add 0x8a000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d)))))

(defcfun emit-or-cr (c r)
	(unless (eq c 0)
		(if (eq c -1)
			(emit-cpy-cr -1 r)
			(progn
				(emit-cpy-cr c r29)
				(emit-or-rr r29 r)))))

(defcfun emit-or-rr (s d)
	(unless (eql s d)
		(emit-int (add 0xaa000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d)))))

(defcfun emit-xor-cr (c r)
	(unless (eq c 0)
		(progn
			(emit-cpy-cr c r29)
			(emit-xor-rr r29 r))))

(defcfun emit-xor-rr (s d)
	(emit-int (add 0xca000000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

(defcfun emit-shl-cr (c r)
	(unless (eq c 0)
		(emit-int (add 0xd3400000 (bit-shl (bit-and (sub 64 c) 63) 16) (bit-shl (sub 63 c) 10)
			(bit-shl (reg r) 5) (reg r)))))

(defcfun emit-shl-rr (s d)
	(emit-int (add 0x9ac02000 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

(defcfun emit-shr-cr (c r)
	(unless (eq c 0)
		(emit-int (add 0xd3400000 (bit-shl c 16) (bit-shl 63 10) (bit-shl (reg r) 5) (reg r)))))

(defcfun emit-shr-rr (s d)
	(emit-int (add 0x9ac02400 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

(defcfun emit-asr-cr (c r)
	(unless (eq c 0)
		(emit-int (add 0x93400000 (bit-shl c 16) (bit-shl 63 10) (bit-shl (reg r) 5) (reg r)))))

(defcfun emit-asr-rr (s d)
	(emit-int (add 0x9ac02800 (bit-shl (reg s) 16) (bit-shl (reg d) 5) (reg d))))

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
	(emit-int (add 0x9b000000 (bit-shl (reg d) 16) (bit-shl 31 10) (bit-shl (reg s) 5) (reg d))))

(defcfun emit-ext-rr (b s d))
(defcfun emit-div-rrr (s d1 d2))
(defcfun emit-div-rrr-u (s d1 d2))
