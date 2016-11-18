;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(equate 'r0 'r0)
(equate 'r1 'r1)
(equate 'r2 'r2)
(equate 'r3 'r3)
(equate 'r4 'r4)
(equate 'r5 'r5)
(equate 'r6 'r6)
(equate 'r7 'r7)
(equate 'r8 'r8)
(equate 'r9 'r9)
(equate 'r10 'r10)
(equate 'r11 'r11)
(equate 'r12 'r12)
(equate 'r13 'r13)
(equate 'r14 'r14)
(equate 'r15 'r15)

(defcompilefun reg (r)
	(defq c 0)
	(some (lambda (x)
		(if (eql r x) c
			(progn (setq c (inc c)) nil)))))

(defcompilefun vp-label (s) (emit `(emit-label ',s)) (def *compile-env* s 0))
(defcompilefun vp-align (&rest b) (emit `(emit-align ~b)))
(defcompilefun vp-string (&rest b) (emit `(emit-string ~b)))
(defcompilefun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defcompilefun vp-short (&rest b) (emit `(emit-short ~b)))
(defcompilefun vp-int (&rest b) (emit `(emit-int ~b)))
(defcompilefun vp-long (&rest b) (emit `(emit-long ~b)))

(defcompilefun vp-beq (&rest b) (emit `(emit-beq ~b)))
(defcompilefun vp-bne (&rest b) (emit `(emit-bne ~b)))
(defcompilefun vp-blt (&rest b) (emit `(emit-blt ~b)))
(defcompilefun vp-ble (&rest b) (emit `(emit-ble ~b)))
(defcompilefun vp-bgt (&rest b) (emit `(emit-bgt ~b)))
(defcompilefun vp-bge (&rest b) (emit `(emit-bge ~b)))

(defcompilefun vp-push (&rest b) (emit `(emit-push ~b)))
(defcompilefun vp-pop (&rest b) (emit `(emit-pop ~b)))

(defcompilefun vp-call (&rest b) (emit `(emit-call ~b)))
(defcompilefun vp-call-r (&rest b) (emit `(emit-call-r ~b)))
(defcompilefun vp-call-i (&rest b) (emit `(emit-call-i ~b)))
(defcompilefun vp-call-d (&rest b) (emit `(emit-call-d ~b)))
(defcompilefun vp-call-p (&rest b) (emit `(emit-call-p ~b)))

(defcompilefun vp-jmp (&rest b) (emit `(emit-jmp ~b)))
(defcompilefun vp-jmp-r (&rest b) (emit `(emit-jmp-r ~b)))
(defcompilefun vp-jmp-i (&rest b) (emit `(emit-jmp-i ~b)))
(defcompilefun vp-jmp-d (&rest b) (emit `(emit-jmp-d ~b)))
(defcompilefun vp-jmp-p (&rest b) (emit `(emit-jmp-p ~b)))

(defcompilefun vp-lea-i (&rest b) (emit `(emit-lea-i ~b)))
(defcompilefun vp-lea-d (&rest b) (emit `(emit-lea-d ~b)))
(defcompilefun vp-lea-p (&rest b) (emit `(emit-lea-p ~b)))

(defcompilefun vp-cpy-cr (&rest b) (emit `(emit-cpy-cr ~b)))
(defcompilefun vp-cpy-rr (&rest b) (emit `(emit-cpy-rr ~b)))
(defcompilefun vp-cpy-ir (&rest b) (emit `(emit-cpy-ir ~b)))
(defcompilefun vp-cpy-dr (&rest b) (emit `(emit-cpy-dr ~b)))
(defcompilefun vp-cpy-pr (&rest b) (emit `(emit-cpy-pr ~b)))
(defcompilefun vp-cpy-ri (&rest b) (emit `(emit-cpy-ri ~b)))
(defcompilefun vp-cpy-rd (&rest b) (emit `(emit-cpy-rd ~b)))
(defcompilefun vp-cpy-rp (&rest b) (emit `(emit-cpy-rp ~b)))

(defcompilefun vp-cpy-ir-b (&rest b) (emit `(emit-cpy-ir-b ~b)))
(defcompilefun vp-cpy-dr-b (&rest b) (emit `(emit-cpy-dr-b ~b)))
(defcompilefun vp-cpy-pr-b (&rest b) (emit `(emit-cpy-pr-b ~b)))
(defcompilefun vp-cpy-ir-ub (&rest b) (emit `(emit-cpy-ir-ub ~b)))
(defcompilefun vp-cpy-dr-ub (&rest b) (emit `(emit-cpy-dr-ub ~b)))
(defcompilefun vp-cpy-pr-ub (&rest b) (emit `(emit-cpy-pr-ub ~b)))

(defcompilefun vp-cpy-ir-s (&rest b) (emit `(emit-cpy-ir-s ~b)))
(defcompilefun vp-cpy-dr-s (&rest b) (emit `(emit-cpy-dr-s ~b)))
(defcompilefun vp-cpy-pr-s (&rest b) (emit `(emit-cpy-pr-s ~b)))
(defcompilefun vp-cpy-ir-us (&rest b) (emit `(emit-cpy-ir-us ~b)))
(defcompilefun vp-cpy-dr-us (&rest b) (emit `(emit-cpy-dr-us ~b)))
(defcompilefun vp-cpy-pr-us (&rest b) (emit `(emit-cpy-pr-us ~b)))

(defcompilefun vp-cpy-ir-i (&rest b) (emit `(emit-cpy-ir-i ~b)))
(defcompilefun vp-cpy-dr-i (&rest b) (emit `(emit-cpy-dr-i ~b)))
(defcompilefun vp-cpy-pr-i (&rest b) (emit `(emit-cpy-pr-i ~b)))
(defcompilefun vp-cpy-ir-ui (&rest b) (emit `(emit-cpy-ir-ui ~b)))
(defcompilefun vp-cpy-dr-ui (&rest b) (emit `(emit-cpy-dr-ui ~b)))
(defcompilefun vp-cpy-pr-ui (&rest b) (emit `(emit-cpy-pr-ui ~b)))

(defcompilefun vp-cpy-ri-b (&rest b) (emit `(emit-cpy-ri-b ~b)))
(defcompilefun vp-cpy-rd-b (&rest b) (emit `(emit-cpy-rd-b ~b)))
(defcompilefun vp-cpy-rp-b (&rest b) (emit `(emit-cpy-rp-b ~b)))

(defcompilefun vp-cpy-ri-s (&rest b) (emit `(emit-cpy-ri-s ~b)))
(defcompilefun vp-cpy-rd-s (&rest b) (emit `(emit-cpy-rd-s ~b)))
(defcompilefun vp-cpy-rp-s (&rest b) (emit `(emit-cpy-rp-s ~b)))

(defcompilefun vp-cpy-ri-i (&rest b) (emit `(emit-cpy-ri-i ~b)))
(defcompilefun vp-cpy-rd-i (&rest b) (emit `(emit-cpy-rd-i ~b)))
(defcompilefun vp-cpy-rp-i (&rest b) (emit `(emit-cpy-rp-i ~b)))

(defcompilefun vp-add-cr (&rest b) (emit `(emit-add-cr ~b)))
(defcompilefun vp-add-rr (&rest b) (emit `(emit-add-rr ~b)))
(defcompilefun vp-add-ir (&rest b) (emit `(emit-add-ir ~b)))
(defcompilefun vp-add-dr (&rest b) (emit `(emit-add-dr ~b)))
(defcompilefun vp-add-pr (&rest b) (emit `(emit-add-pr ~b)))

(defcompilefun vp-sub-cr (&rest b) (emit `(emit-sub-cr ~b)))
(defcompilefun vp-sub-rr (&rest b) (emit `(emit-sub-rr ~b)))
(defcompilefun vp-sub-ir (&rest b) (emit `(emit-sub-ir ~b)))
(defcompilefun vp-sub-dr (&rest b) (emit `(emit-sub-dr ~b)))
(defcompilefun vp-sub-pr (&rest b) (emit `(emit-sub-pr ~b)))

(defcompilefun vp-cmp-cr (&rest b) (emit `(emit-cmp-cr ~b)))
(defcompilefun vp-cmp-rr (&rest b) (emit `(emit-cmp-rr ~b)))
(defcompilefun vp-cmp-ir (&rest b) (emit `(emit-cmp-ir ~b)))
(defcompilefun vp-cmp-dr (&rest b) (emit `(emit-cmp-dr ~b)))
(defcompilefun vp-cmp-pr (&rest b) (emit `(emit-cmp-pr ~b)))

(defcompilefun vp-mul-cr (&rest b) (emit `(emit-mul-cr ~b)))
(defcompilefun vp-mul-rr (&rest b) (emit `(emit-mul-rr ~b)))
(defcompilefun vp-mul-ir (&rest b) (emit `(emit-mul-ir ~b)))
(defcompilefun vp-mul-dr (&rest b) (emit `(emit-mul-dr ~b)))
(defcompilefun vp-mul-pr (&rest b) (emit `(emit-mul-pr ~b)))

(defcompilefun vp-and-cr (&rest b) (emit `(emit-and-cr ~b)))
(defcompilefun vp-and-rr (&rest b) (emit `(emit-and-rr ~b)))
(defcompilefun vp-and-ir (&rest b) (emit `(emit-and-ir ~b)))
(defcompilefun vp-and-dr (&rest b) (emit `(emit-and-dr ~b)))
(defcompilefun vp-and-pr (&rest b) (emit `(emit-and-pr ~b)))

(defcompilefun vp-or-cr (&rest b) (emit `(emit-or-cr ~b)))
(defcompilefun vp-or-rr (&rest b) (emit `(emit-or-rr ~b)))
(defcompilefun vp-or-ir (&rest b) (emit `(emit-or-ir ~b)))
(defcompilefun vp-or-dr (&rest b) (emit `(emit-or-dr ~b)))
(defcompilefun vp-or-pr (&rest b) (emit `(emit-or-pr ~b)))

(defcompilefun vp-xor-cr (&rest b) (emit `(emit-xor-cr ~b)))
(defcompilefun vp-xor-rr (&rest b) (emit `(emit-xor-rr ~b)))
(defcompilefun vp-xor-ir (&rest b) (emit `(emit-xor-ir ~b)))
(defcompilefun vp-xor-dr (&rest b) (emit `(emit-xor-dr ~b)))
(defcompilefun vp-xor-pr (&rest b) (emit `(emit-xor-pr ~b)))

(defcompilefun vp-ret () (emit `(emit-ret)))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defcompilefun emit (&rest b)
	(each (lambda (x)
		(push *emit-buffer* x)) b))

(defcompilefun emit-passes ()
	(defq *out-buffer-cnt* 0 *out-buffer-size* 0)
	(while (ne 2 *out-buffer-cnt*)
		(setq *out-buffer* (list))
		(each eval *emit-buffer*)
		(setq *out-buffer-cnt* (if (eq *out-buffer-size* (length *out-buffer*))
			(inc *out-buffer-cnt*)
			(progn (setq *out-buffer-size* (length *out-buffer*)) 0)))))

(defcompilefun print-emit-buffer ()
	(defq i 0)
	(while (lt i (length *emit-buffer*))
		(print i " -> " (elem i *emit-buffer*))
		(setq i (inc i))))

(defcompilefun print-out-buffer (c)
	(defq i 0)
	(while (lt i (length *out-buffer*))
		(if (eq (mod i c) 0)
			(progn
				(prin-base i 16 4) (prin " : ")))
		(prin-base (elem i *out-buffer*) 16 2) (prin " ")
		(setq i (inc i))
		(if (eq (mod i c) 0)
			(print)))
	(print))

(defcompilefun emit-label (s)
	(set s (length *out-buffer*)))

(defcompilefun emit-byte (&rest b)
	(each (lambda (x)
		(push *out-buffer* (bit-and x 0xff))) b))

(defcompilefun emit-short (&rest b)
	(each (lambda (x)
		(emit-byte x (bit-shr x 8))) b))

(defcompilefun emit-int (&rest b)
	(each (lambda (x)
		(emit-short x (bit-shr x 16))) b))

(defcompilefun emit-long (&rest b)
	(each (lambda (x)
		(emit-int x (bit-shr x 32))) b))

(defcompilefun emit-string (s)
	(each (lambda (x)
		(emit-byte (code x))) s))

(defcompilefun emit-align (a &optional b)
	(defq n (align (length *out-buffer*) a) b (if b b 0))
	(while (ne (length *out-buffer*) n)
		(emit-byte b)))

;;;;;;;;;;;;;;;;;;;;
; x64 Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defcompilefun emit-beq (%1) ())
(defcompilefun emit-bne (%1) ())
(defcompilefun emit-blt (%1) ())
(defcompilefun emit-ble (%1) ())
(defcompilefun emit-bgt (%1) ())
(defcompilefun emit-bge (%1) ())

(defcompilefun emit-push (&rest b) ())
(defcompilefun emit-pop (&rest b) ())

(defcompilefun emit-call (&rest b) ())
(defcompilefun emit-call-r (&rest b) ())
(defcompilefun emit-call-i (&rest b) ())
(defcompilefun emit-call-d (&rest b) ())
(defcompilefun emit-call-p (&rest b) ())

(defcompilefun emit-jmp (&rest b) ())
(defcompilefun emit-jmp-r (&rest b) ())
(defcompilefun emit-jmp-i (&rest b) ())
(defcompilefun emit-jmp-d (&rest b) ())
(defcompilefun emit-jmp-p (&rest b) ())

(defcompilefun emit-lea-i (&rest b) ())
(defcompilefun emit-lea-d (&rest b) ())
(defcompilefun emit-lea-p (&rest b) ())

(defcompilefun emit-cpy-cr (&rest b) ())
(defcompilefun emit-cpy-rr (&rest b) ())
(defcompilefun emit-cpy-ir (&rest b) ())
(defcompilefun emit-cpy-dr (&rest b) ())
(defcompilefun emit-cpy-pr (&rest b) ())
(defcompilefun emit-cpy-ri (&rest b) ())
(defcompilefun emit-cpy-rd (&rest b) ())
(defcompilefun emit-cpy-rp (&rest b) ())

(defcompilefun emit-cpy-ir-b (&rest b) ())
(defcompilefun emit-cpy-dr-b (&rest b) ())
(defcompilefun emit-cpy-pr-b (&rest b) ())
(defcompilefun emit-cpy-ir-ub (&rest b) ())
(defcompilefun emit-cpy-dr-ub (&rest b) ())
(defcompilefun emit-cpy-pr-ub (&rest b) ())

(defcompilefun emit-cpy-ir-s (&rest b) ())
(defcompilefun emit-cpy-dr-s (&rest b) ())
(defcompilefun emit-cpy-pr-s (&rest b) ())
(defcompilefun emit-cpy-ir-us (&rest b) ())
(defcompilefun emit-cpy-dr-us (&rest b) ())
(defcompilefun emit-cpy-pr-us (&rest b) ())

(defcompilefun emit-cpy-ir-i (&rest b) ())
(defcompilefun emit-cpy-dr-i (&rest b) ())
(defcompilefun emit-cpy-pr-i (&rest b) ())
(defcompilefun emit-cpy-ir-ui (&rest b) ())
(defcompilefun emit-cpy-dr-ui (&rest b) ())
(defcompilefun emit-cpy-pr-ui (&rest b) ())

(defcompilefun emit-cpy-ri-b (&rest b) ())
(defcompilefun emit-cpy-rd-b (&rest b) ())
(defcompilefun emit-cpy-rp-b (&rest b) ())

(defcompilefun emit-cpy-ri-s (&rest b) ())
(defcompilefun emit-cpy-rd-s (&rest b) ())
(defcompilefun emit-cpy-rp-s (&rest b) ())

(defcompilefun emit-cpy-ri-i (&rest b) ())
(defcompilefun emit-cpy-rd-i (&rest b) ())
(defcompilefun emit-cpy-rp-i (&rest b) ())

(defcompilefun emit-add-cr (&rest b) ())
(defcompilefun emit-add-rr (&rest b) ())
(defcompilefun emit-add-ir (&rest b) ())
(defcompilefun emit-add-dr (&rest b) ())
(defcompilefun emit-add-pr (&rest b) ())

(defcompilefun emit-sub-cr (&rest b) ())
(defcompilefun emit-sub-rr (&rest b) ())
(defcompilefun emit-sub-ir (&rest b) ())
(defcompilefun emit-sub-dr (&rest b) ())
(defcompilefun emit-sub-pr (&rest b) ())

(defcompilefun emit-cmp-cr (&rest b) ())
(defcompilefun emit-cmp-rr (&rest b) ())
(defcompilefun emit-cmp-ir (&rest b) ())
(defcompilefun emit-cmp-dr (&rest b) ())
(defcompilefun emit-cmp-pr (&rest b) ())

(defcompilefun emit-mul-cr (&rest b) ())
(defcompilefun emit-mul-rr (&rest b) ())
(defcompilefun emit-mul-ir (&rest b) ())
(defcompilefun emit-mul-dr (&rest b) ())
(defcompilefun emit-mul-pr (&rest b) ())

(defcompilefun emit-and-cr (&rest b) ())
(defcompilefun emit-and-rr (&rest b) ())
(defcompilefun emit-and-ir (&rest b) ())
(defcompilefun emit-and-dr (&rest b) ())
(defcompilefun emit-and-pr (&rest b) ())

(defcompilefun emit-or-cr (&rest b) ())
(defcompilefun emit-or-rr (&rest b) ())
(defcompilefun emit-or-ir (&rest b) ())
(defcompilefun emit-or-dr (&rest b) ())
(defcompilefun emit-or-pr (&rest b) ())

(defcompilefun emit-xor-cr (&rest b) ())
(defcompilefun emit-xor-rr (&rest b) ())
(defcompilefun emit-xor-ir (&rest b) ())
(defcompilefun emit-xor-dr (&rest b) ())
(defcompilefun emit-xor-pr (&rest b) ())

(defcompilefun emit-ret () (emit-byte 0xc3))
