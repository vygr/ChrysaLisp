;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(defq r0 0 r1 1 r2 2 r3 3 r4 4 r5 5 r6 6 r7 7 r8 8
	r9 9 r10 10 r11 11 r12 12 r13 3 r14 14 r15 15)

(defun vp-label (s) (emit `(emit-label ',s)) (def *compile-env* s 0))
(defun vp-align (&rest b) (emit `(emit-align ~b)))
(defun vp-string (&rest b) (emit `(emit-string ~b)))
(defun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defun vp-short (&rest b) (emit `(emit-short ~b)))
(defun vp-int (&rest b) (emit `(emit-int ~b)))
(defun vp-long (&rest b) (emit `(emit-long ~b)))
(defun vp-push (&rest b) (emit `(emit-push ~b)))
(defun vp-pop (&rest b) (emit `(emit-pop ~b)))
(defun vp-rel (&rest b) (emit `(emit-rel ~b)))
(defun vp-call-rel (&rest b) (emit `(emit-call-rel ~b)))
(defun vp-jmp-rel (&rest b) (emit `(emit-jmp-rel ~b)))
(defun vp-cpy-rel-r (&rest b) (emit `(emit-cpy-rel-r ~b)))
(defun vp-cpy-r-rel (&rest b) (emit `(emit-cpy-r-rel ~b)))
(defun vp-cpy-rr (&rest b) (emit `(emit-cpy-rr ~b)))
(defun vp-cpy-ri (&rest b) (emit `(emit-cpy-ri ~b)))
(defun vp-cpy-ir (&rest b) (emit `(emit-cpy-ir ~b)))
(defun vp-add-cr (&rest b) (emit `(emit-add-cr ~b)))
(defun vp-add-rr (&rest b) (emit `(emit-add-rr ~b)))
(defun vp-sub-cr (&rest b) (emit `(emit-sub-cr ~b)))
(defun vp-sub-rr (&rest b) (emit `(emit-sub-rr ~b)))
(defun vp-mul-cr (&rest b) (emit `(emit-mul-cr ~b)))
(defun vp-mul-rr (&rest b) (emit `(emit-mul-rr ~b)))
(defun vp-and-cr (&rest b) (emit `(emit-and-cr ~b)))
(defun vp-and-rr (&rest b) (emit `(emit-and-rr ~b)))
(defun vp-or-cr (&rest b) (emit `(emit-or-cr ~b)))
(defun vp-or-rr (&rest b) (emit `(emit-or-rr ~b)))
(defun vp-xor-cr (&rest b) (emit `(emit-xor-cr ~b)))
(defun vp-xor-rr (&rest b) (emit `(emit-xor-rr ~b)))
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
