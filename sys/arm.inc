;;;;;;;;;;;;;;;;;;;;
; ARM Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defcfun reg (r)
	(find r '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
			_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ rsp)))

(defcvar 'stack_align 16)
(defcfun emit-alloc (c) (emit-sub-cr (align c stack_align) rsp))
(defcfun emit-free (c) (emit-add-cr (align c stack_align) rsp))

(defcfun emit-ret ())

(defcfun emit-push (&rest b)
	(emit-sub-cr (align (mul ptr_size (length b)) stack_align) rsp)
	(each! nil nil nil (lambda (r)
		(emit-cpy-ri r rsp (mul _ ptr_size))) (list b)))

(defcfun emit-pop (&rest b)
	(each! -1 0 nil (lambda (r)
		(emit-cpy-ir rsp (mul _ ptr_size) r)) (list b))
	(emit-add-cr (align (mul ptr_size (length b)) stack_align) rsp))

(defcfun emit-syscall ())

(defcfun emit-branch (o c d)
	(defq m (elem d *distance*))
	(if (ne *pass* 0) (if (gt (abs c) (abs m)) (elem-set d *distance* (setq m c))))
	(cond
		((le -0x80 (sub m 2) 0x7f)
			(emit-byte o (sub c 2)))
		((le -0x80000000 (sub m 6) 0x7fffffff)
			(emit-int o (sub c 6)))
		(t (print "emit-branch constant out of range: " c))))

(defcfun emit-beq (l d) (emit-branch 0x1 (sub l *pc*) d))
(defcfun emit-bne (l d) (emit-branch 0x2 (sub l *pc*) d))
(defcfun emit-blt (l d) (emit-branch 0x3 (sub l *pc*) d))
(defcfun emit-ble (l d) (emit-branch 0x4 (sub l *pc*) d))
(defcfun emit-bgt (l d) (emit-branch 0x5 (sub l *pc*) d))
(defcfun emit-bge (l d) (emit-branch 0x6 (sub l *pc*) d))

(defcfun emit-call (l)
	(defq c (sub l *pc* 5))
	(cond
		((le -0x80000000 c 0x7fffffff)
			(emit-int c))
		(t (print "emit-call constant out of range: " c))))

(defcfun emit-call-r (r))
(defcfun emit-call-i (d c))
(defcfun emit-call-p (l))

(defcfun emit-jmp (l d))
(defcfun emit-jmp-r (r))
(defcfun emit-jmp-i (d c))
(defcfun emit-jmp-p (l))

(defcfun emit-lea-i (s c d))
(defcfun emit-lea-d (s1 s2 d))
(defcfun emit-lea-p (l d))

(defcfun emit-cpy-cr (c r))
(defcfun emit-cpy-rr (s d) (unless (eql s d)))
(defcfun emit-cpy-ir (s c d))
(defcfun emit-cpy-dr (s1 s2 d))
(defcfun emit-cpy-pr (l d))

(defcfun emit-cpy-ri (s d c))
(defcfun emit-cpy-rd (d s1 s2))
(defcfun emit-cpy-rp (d c))

(defcfun emit-cpy-ir-b (s c d))
(defcfun emit-cpy-dr-b (s1 s2 d))
(defcfun emit-cpy-ir-ub (s c d))
(defcfun emit-cpy-dr-ub (s1 s2 d))

(defcfun emit-cpy-ir-s (s c d))
(defcfun emit-cpy-dr-s (s1 s2 d))
(defcfun emit-cpy-ir-us (s c d))
(defcfun emit-cpy-dr-us (s1 s2 d))

(defcfun emit-cpy-ir-i (s c d))
(defcfun emit-cpy-dr-i (s1 s2 d))
(defcfun emit-cpy-ir-ui (s c d))
(defcfun emit-cpy-dr-ui (s1 s2 d))

(defcfun emit-cpy-ri-b (s d c))
(defcfun emit-cpy-rd-b (r s1 s2))
(defcfun emit-cpy-ri-s (s d c))
(defcfun emit-cpy-rd-s (r s1 s2))
(defcfun emit-cpy-ri-i (s d c))
(defcfun emit-cpy-rd-i (r s1 s2))

(defcfun emit-add-cr (c r))
(defcfun emit-add-rr (s d))
(defcfun emit-sub-cr (c r))
(defcfun emit-sub-rr (s d))
(defcfun emit-cmp-cr (c r))
(defcfun emit-cmp-rr (s d))
(defcfun emit-and-cr (c r))
(defcfun emit-and-rr (s d) (unless (eql s d)))
(defcfun emit-or-cr (c r))
(defcfun emit-or-rr (s d) (unless (eql s d)))
(defcfun emit-xor-cr (c r))
(defcfun emit-xor-rr (s d))
(defcfun emit-mul-cr (c r))
(defcfun emit-mul-rr (s d))

(defcfun emit-shl-cr (c r))
(defcfun emit-shl-rr (s d))
(defcfun emit-shr-cr (c r))
(defcfun emit-shr-rr (s d))
(defcfun emit-asr-cr (c r))
(defcfun emit-asr-rr (s d))

(defcfun emit-xchg-rr (s d) (unless (eql s d)))
(defcfun emit-ext-rr (b s d))

(defcfun emit-div-rrr (s d1 d2))
(defcfun emit-div-rrr-u (s d1 d2))
