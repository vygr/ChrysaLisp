;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(equate 'r0 'r0 'r1 'r1 'r2 'r2 'r3 'r3 'r4 'r4 'r5 'r5 'r6 'r6 'r7 'r7 'r8 'r8
	'r9 'r9 'r10 'r10 'r11 'r11 'r12 'r12 'r13 'r13 'r14 'r14 'r15 'r15)

(defun reg (r)
	(defq c 0)
	(some (lambda (x)
		(if (eql r x) c
			(progn (setq c (inc c)) nil)))))

(defun vp-label (s) (emit `(emit-label ',s)) (def *compile-env* s 0))
(defun vp-align (&rest b) (emit `(emit-align ~b)))
(defun vp-string (&rest b) (emit `(emit-string ~b)))
(defun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defun vp-short (&rest b) (emit `(emit-short ~b)))
(defun vp-int (&rest b) (emit `(emit-int ~b)))
(defun vp-long (&rest b) (emit `(emit-long ~b)))

(defun vp-beq (&rest b) (emit `(emit-beq ~b)))
(defun vp-bne (&rest b) (emit `(emit-bne ~b)))
(defun vp-blt (&rest b) (emit `(emit-blt ~b)))
(defun vp-ble (&rest b) (emit `(emit-ble ~b)))
(defun vp-bgt (&rest b) (emit `(emit-bgt ~b)))
(defun vp-bge (&rest b) (emit `(emit-bge ~b)))

(defun vp-push (&rest b) (emit `(emit-push ~b)))
(defun vp-pop (&rest b) (emit `(emit-pop ~b)))

(defun vp-call (&rest b) (emit `(emit-call ~b)))
(defun vp-call-r (&rest b) (emit `(emit-call-r ~b)))
(defun vp-call-i (&rest b) (emit `(emit-call-i ~b)))
(defun vp-call-d (&rest b) (emit `(emit-call-d ~b)))
(defun vp-call-p (&rest b) (emit `(emit-call-p ~b)))

(defun vp-jmp (&rest b) (emit `(emit-jmp ~b)))
(defun vp-jmp-r (&rest b) (emit `(emit-jmp-r ~b)))
(defun vp-jmp-i (&rest b) (emit `(emit-jmp-i ~b)))
(defun vp-jmp-d (&rest b) (emit `(emit-jmp-d ~b)))
(defun vp-jmp-p (&rest b) (emit `(emit-jmp-p ~b)))

(defun vp-lea-i (&rest b) (emit `(emit-lea-i ~b)))
(defun vp-lea-d (&rest b) (emit `(emit-lea-d ~b)))
(defun vp-lea-p (&rest b) (emit `(emit-lea-p ~b)))

(defun vp-cpy-cr (&rest b) (emit `(emit-cpy-cr ~b)))
(defun vp-cpy-rr (&rest b) (emit `(emit-cpy-rr ~b)))
(defun vp-cpy-ir (&rest b) (emit `(emit-cpy-ir ~b)))
(defun vp-cpy-dr (&rest b) (emit `(emit-cpy-dr ~b)))
(defun vp-cpy-pr (&rest b) (emit `(emit-cpy-pr ~b)))
(defun vp-cpy-ri (&rest b) (emit `(emit-cpy-ri ~b)))
(defun vp-cpy-rd (&rest b) (emit `(emit-cpy-rd ~b)))
(defun vp-cpy-rp (&rest b) (emit `(emit-cpy-rp ~b)))

(defun vp-cpy-ir-b (&rest b) (emit `(emit-cpy-ir-b ~b)))
(defun vp-cpy-dr-b (&rest b) (emit `(emit-cpy-dr-b ~b)))
(defun vp-cpy-pr-b (&rest b) (emit `(emit-cpy-pr-b ~b)))
(defun vp-cpy-ir-ub (&rest b) (emit `(emit-cpy-ir-ub ~b)))
(defun vp-cpy-dr-ub (&rest b) (emit `(emit-cpy-dr-ub ~b)))
(defun vp-cpy-pr-ub (&rest b) (emit `(emit-cpy-pr-ub ~b)))

(defun vp-cpy-ir-s (&rest b) (emit `(emit-cpy-ir-s ~b)))
(defun vp-cpy-dr-s (&rest b) (emit `(emit-cpy-dr-s ~b)))
(defun vp-cpy-pr-s (&rest b) (emit `(emit-cpy-pr-s ~b)))
(defun vp-cpy-ir-us (&rest b) (emit `(emit-cpy-ir-us ~b)))
(defun vp-cpy-dr-us (&rest b) (emit `(emit-cpy-dr-us ~b)))
(defun vp-cpy-pr-us (&rest b) (emit `(emit-cpy-pr-us ~b)))

(defun vp-cpy-ir-i (&rest b) (emit `(emit-cpy-ir-i ~b)))
(defun vp-cpy-dr-i (&rest b) (emit `(emit-cpy-dr-i ~b)))
(defun vp-cpy-pr-i (&rest b) (emit `(emit-cpy-pr-i ~b)))
(defun vp-cpy-ir-ui (&rest b) (emit `(emit-cpy-ir-ui ~b)))
(defun vp-cpy-dr-ui (&rest b) (emit `(emit-cpy-dr-ui ~b)))
(defun vp-cpy-pr-ui (&rest b) (emit `(emit-cpy-pr-ui ~b)))

(defun vp-cpy-ri-b (&rest b) (emit `(emit-cpy-ri-b ~b)))
(defun vp-cpy-rd-b (&rest b) (emit `(emit-cpy-rd-b ~b)))
(defun vp-cpy-rp-b (&rest b) (emit `(emit-cpy-rp-b ~b)))

(defun vp-cpy-ri-s (&rest b) (emit `(emit-cpy-ri-s ~b)))
(defun vp-cpy-rd-s (&rest b) (emit `(emit-cpy-rd-s ~b)))
(defun vp-cpy-rp-s (&rest b) (emit `(emit-cpy-rp-s ~b)))

(defun vp-cpy-ri-i (&rest b) (emit `(emit-cpy-ri-i ~b)))
(defun vp-cpy-rd-i (&rest b) (emit `(emit-cpy-rd-i ~b)))
(defun vp-cpy-rp-i (&rest b) (emit `(emit-cpy-rp-i ~b)))

(defun vp-add-cr (&rest b) (emit `(emit-add-cr ~b)))
(defun vp-add-rr (&rest b) (emit `(emit-add-rr ~b)))
(defun vp-add-ir (&rest b) (emit `(emit-add-ir ~b)))
(defun vp-add-dr (&rest b) (emit `(emit-add-dr ~b)))
(defun vp-add-pr (&rest b) (emit `(emit-add-pr ~b)))

(defun vp-sub-cr (&rest b) (emit `(emit-sub-cr ~b)))
(defun vp-sub-rr (&rest b) (emit `(emit-sub-rr ~b)))
(defun vp-sub-ir (&rest b) (emit `(emit-sub-ir ~b)))
(defun vp-sub-dr (&rest b) (emit `(emit-sub-dr ~b)))
(defun vp-sub-pr (&rest b) (emit `(emit-sub-pr ~b)))

(defun vp-cmp-cr (&rest b) (emit `(emit-cmp-cr ~b)))
(defun vp-cmp-rr (&rest b) (emit `(emit-cmp-rr ~b)))
(defun vp-cmp-ir (&rest b) (emit `(emit-cmp-ir ~b)))
(defun vp-cmp-dr (&rest b) (emit `(emit-cmp-dr ~b)))
(defun vp-cmp-pr (&rest b) (emit `(emit-cmp-pr ~b)))

(defun vp-mul-cr (&rest b) (emit `(emit-mul-cr ~b)))
(defun vp-mul-rr (&rest b) (emit `(emit-mul-rr ~b)))
(defun vp-mul-ir (&rest b) (emit `(emit-mul-ir ~b)))
(defun vp-mul-dr (&rest b) (emit `(emit-mul-dr ~b)))
(defun vp-mul-pr (&rest b) (emit `(emit-mul-pr ~b)))

(defun vp-and-cr (&rest b) (emit `(emit-and-cr ~b)))
(defun vp-and-rr (&rest b) (emit `(emit-and-rr ~b)))
(defun vp-and-ir (&rest b) (emit `(emit-and-ir ~b)))
(defun vp-and-dr (&rest b) (emit `(emit-and-dr ~b)))
(defun vp-and-pr (&rest b) (emit `(emit-and-pr ~b)))

(defun vp-or-cr (&rest b) (emit `(emit-or-cr ~b)))
(defun vp-or-rr (&rest b) (emit `(emit-or-rr ~b)))
(defun vp-or-ir (&rest b) (emit `(emit-or-ir ~b)))
(defun vp-or-dr (&rest b) (emit `(emit-or-dr ~b)))
(defun vp-or-pr (&rest b) (emit `(emit-or-pr ~b)))

(defun vp-xor-cr (&rest b) (emit `(emit-xor-cr ~b)))
(defun vp-xor-rr (&rest b) (emit `(emit-xor-rr ~b)))
(defun vp-xor-ir (&rest b) (emit `(emit-xor-ir ~b)))
(defun vp-xor-dr (&rest b) (emit `(emit-xor-dr ~b)))
(defun vp-xor-pr (&rest b) (emit `(emit-xor-pr ~b)))

(defun vp-ret () (emit `(emit-ret)))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defun emit (&rest b)
	(each (lambda (x)
		(push *emit-buffer* x)) b))

(defun emit-passes ()
	(defq *out-buffer-cnt* 0 *out-buffer-size* 0)
	(while (ne 2 *out-buffer-cnt*)
		(setq *out-buffer* (list))
		(each eval *emit-buffer*)
		(setq *out-buffer-cnt* (if (eq *out-buffer-size* (length *out-buffer*))
			(inc *out-buffer-cnt*)
			(progn (setq *out-buffer-size* (length *out-buffer*)) 0)))))

(defun print-emit-buffer ()
	(defq i 0)
	(while (lt i (length *emit-buffer*))
		(print i " -> " (elem i *emit-buffer*))
		(setq i (inc i))))

(defun print-out-buffer (c)
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

(defun emit-label (s)
	(set s (length *out-buffer*)))

(defun emit-byte (&rest b)
	(each (lambda (x)
		(push *out-buffer* (bit-and x 0xff))) b))

(defun emit-short (&rest b)
	(each (lambda (x)
		(emit-byte x (bit-shr x 8))) b))

(defun emit-int (&rest b)
	(each (lambda (x)
		(emit-short x (bit-shr x 16))) b))

(defun emit-long (&rest b)
	(each (lambda (x)
		(emit-int x (bit-shr x 32))) b))

(defun emit-string (s)
	(each (lambda (x)
		(emit-byte (code x))) s))

(defun emit-align (a &optional b)
	(defq n (align (length *out-buffer*) a) b (if b b 0))
	(while (ne (length *out-buffer*) n)
		(emit-byte b)))

;;;;;;;;;;;;;;;;;;;;
; x64 Emit Functions
;;;;;;;;;;;;;;;;;;;;

(defun emit-push (&rest b)
	(emit-byte 0x5)
	(each emit-byte b))

(defun emit-pop (&rest b)
	(emit-byte 0x6)
	(each emit-byte b))

(defun emit-rel (x y)
	(emit-byte 0x70 y)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-call-rel (x)
	(emit-byte 0x71)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-jmp-rel (x)
	(emit-byte 0x72)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-cpy-rel-r (x y)
	(emit-byte 0x73 y)
	(emit-int (sub x (length *out-buffer*) int_size)))

(defun emit-cpy-r-rel (x y)
	(emit-byte 0x74 x)
	(emit-int (sub y (length *out-buffer*) int_size)))

(defun emit-cpy-rr (x y)
	(emit-byte 0x0 x y))

(defun emit-cpy-ri (x y z)
	(emit-byte 0x1 x y)
	(emit-int z))

(defun emit-cpy-ir (x y z)
	(emit-byte 0x2 x z)
	(emit-int y))

(defun emit-add-cr (c x)
	(emit-byte 0x11 x)
	(emit-long c))

(defun emit-add-rr (x y)
	(emit-byte 0x12 x y))

(defun emit-sub-cr (c x)
	(emit-byte 0x13 x)
	(emit-long c))

(defun emit-sub-rr (x y)
	(emit-byte 0x14 x y))

(defun emit-and-cr (c x)
	(emit-byte 0x15 x)
	(emit-long c))

(defun emit-and-rr (x y)
	(emit-byte 0x16 x y))

(defun emit-or-cr (c x)
	(emit-byte 0x17 x)
	(emit-long c))

(defun emit-or-rr (x y)
	(emit-byte 0x18 x y))

(defun emit-xor-cr (c x)
	(emit-byte 0x19 x)
	(emit-long c))

(defun emit-xor-rr (x y)
	(emit-byte 0x19 x y))

(defun emit-mul-cr (c x)
	(emit-byte 0x20 x)
	(emit-long c))

(defun emit-mul-rr (x y)
	(emit-byte 0x21 x y))

(defun emit-ret ()
	(emit-byte 0xc3))
