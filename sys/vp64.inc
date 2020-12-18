;;;;;;;;;;;;;;;;;;;;;
; VP64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(defun reg (r)
	(find-rev r '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 rsp)))

(defcvar 'stack_align 16 'stack_state '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))

(defun stack-init ()
	(defq tk_state_size (* ptr_size (length stack_state)))
	(vp-sub-cr (+ tk_state_size (* ptr_size 2)) r1)
	(f-bind 'sys_task :stop r2)
	(vp-cpy-ri r2 r1 (+ tk_state_size ptr_size))
	(vp-cpy-ri r4 r1 tk_state_size))

(defun emit-push (&rest b))
(defun emit-pop (&rest b))

(defun emit-beq (l d))
(defun emit-bne (l d))
(defun emit-bge (l d))
(defun emit-blt (l d))
(defun emit-bgt (l d))
(defun emit-ble (l d))

(defun emit-seq-cr (c d))
(defun emit-sne-cr (c d))
(defun emit-slt-cr (c d))
(defun emit-sle-cr (c d))
(defun emit-sgt-cr (c d))
(defun emit-sge-cr (c d))

(defun emit-seq-rr (s d))
(defun emit-sne-rr (s d))
(defun emit-slt-rr (s d))
(defun emit-sle-rr (s d))
(defun emit-sgt-rr (s d))
(defun emit-sge-rr (s d))

(defun emit-call (l))
(defun emit-call-r (r))
(defun emit-call-i (d c))
(defun emit-call-p (l))
(defun emit-jmp (l d))
(defun emit-jmp-r (r))
(defun emit-jmp-i (d c))
(defun emit-jmp-p (l))

(defun emit-lea-i (s c d))
(defun emit-lea-d (s1 s2 d))
(defun emit-lea-p (l d))

(defun emit-cpy-ir (s c d))
(defun emit-cpy-ir-b (s c d))
(defun emit-cpy-ir-s (s c d))
(defun emit-cpy-ir-i (s c d))

(defun emit-cpy-ri (s d c))
(defun emit-cpy-ri-b (s d c))
(defun emit-cpy-ri-s (s d c))
(defun emit-cpy-ri-i (s d c))

(defun emit-cpy-ir-ub (s c d))
(defun emit-cpy-ir-us (s c d))
(defun emit-cpy-ir-ui (s c d))

(defun emit-cpy-rd (s d1 d2))
(defun emit-cpy-rd-b (s d2 d1))
(defun emit-cpy-rd-s (s d2 d1))
(defun emit-cpy-rd-i (s d2 d1))

(defun emit-cpy-dr (s1 s2 d))
(defun emit-cpy-dr-b (s1 s2 d))
(defun emit-cpy-dr-s (s1 s2 d))
(defun emit-cpy-dr-i (s1 s2 d))
(defun emit-cpy-dr-ub (s1 s2 d))
(defun emit-cpy-dr-us (s1 s2 d))
(defun emit-cpy-dr-ui (s1 s2 d))

(defun emit-cpy-rp (d l))
(defun emit-cpy-pr (l d))

(defun emit-cpy-cr (c r))
(defun emit-add-cr (c r))
(defun emit-sub-cr (c r))
(defun emit-cmp-cr (c r))
(defun emit-and-cr (c r))
(defun emit-or-cr (c r))
(defun emit-xor-cr (c r))

(defun emit-cpy-rr (s d) (emit-short (+ 0x0 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-add-rr (s d) (emit-short (+ 0x1 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-sub-rr (s d) (emit-short (+ 0x2 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-cmp-rr (s d) (emit-short (+ 0x3 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-and-rr (s d) (emit-short (+ 0x4 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-or-rr (s d) (emit-short (+ 0x5 (<< (+ (reg s) (<< (reg d) 4)) 8))))
(defun emit-xor-rr (s d) (emit-short (+ 0x6 (<< (+ (reg s) (<< (reg d) 4)) 8))))

(defun emit-lnot-rr (r d))
(defun emit-land-rr (s d))

(defun emit-shl-cr (c r))
(defun emit-shr-cr (c r))
(defun emit-asr-cr (c r))

(defun emit-shl-rr (s d))
(defun emit-shr-rr (s d))
(defun emit-asr-rr (s d))

(defun emit-mul-cr (c r))
(defun emit-mul-rr (s d))

(defun emit-swp-rr (s d))
(defun emit-ext-rr (s d))

(defun emit-div-rrr (s d1 d2))
(defun emit-div-rrr-u (s d1 d2))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) rsp))
(defun emit-free (c) (emit-add-cr (align c stack_align) rsp))
(defun emit-ret ())
