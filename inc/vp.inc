;;;;;;;;;;;;;;;;;
; VP Instructions
;;;;;;;;;;;;;;;;;

(defcvar 'r0 'r0 'r1 'r1 'r2 'r2 'r3 'r3 'r4 'r4 'r5 'r5 'r6 'r6 'r7 'r7 'r8 'r8
	'r9 'r9 'r10 'r10 'r11 'r11 'r12 'r12 'r13 'r13 'r14 'r14 'r15 'r15
	'*distance* nil)

(defcfun type (o)
	(defq i (obj? o))
	(cond
		((not i) i)
		((eql i 'class/class_symbol)
			(cond
				((reg o) 'r)
				(t 'c)))
		((eql i 'class/class_vector)
			(setq i (elem 0 o))
			(cond
				((reg i)
					(if (reg (elem 1 o)) 'd
						(if (eq 2 (length o)) 'i
							(sym-cat "i" (elem 2 o)))))
				((eql i '@) i)
				((eql i '$) i)
				((eql i '&)
					(if (reg (elem 1 o))
						(if (reg (elem 2 o)) '&d '&i) nil))
				(t 'c)))
		((eql i 'class/class_string) 's)
		((eql i 'class/class_boxed_long) 'c)
		(t nil)))

(defcfun label-sym (s)
	(sym-cat "_lb_" s))

(defcfun vp-label (s) (setq s (label-sym s)) (emit `(emit-label ',s)) (defcvar s 0))
(defcfun vp-align (&rest b) (emit `(emit-align ~b)))
(defcfun vp-string (&rest b) (emit `(emit-string ~b)))
(defcfun vp-byte (&rest b) (emit `(emit-byte ~b)))
(defcfun vp-short (&rest b) (emit `(emit-short ~b)))
(defcfun vp-int (&rest b) (emit `(emit-int ~b)))
(defcfun vp-long (&rest b) (emit `(emit-long ~b)))

(defcfun vp-jmpif (b l)
	(defq d (elem 0 b) o (elem 1 b) s (elem 2 b) c (type s))
	(cond
		((eql c 'c) (vp-cmp-cr s d))
		((eql c 'r) (vp-cmp-rr s d))
		(t (error "No such compare mode" c)))
	(cond
		((eql o '==) (vp-beq l))
		((eql o '!=) (vp-bne l))
		((eql o '<) (vp-blt l))
		((eql o '>) (vp-bgt l))
		((eql o '<=) (vp-ble l))
		((eql o '>=) (vp-bge l))
		(t (error "No such comparator" o))))

(defcfun vp-jmpifnot (b l)
	(defq d (elem 0 b) o (elem 1 b) s (elem 2 b) c (type s))
	(cond
		((eql c 'c) (vp-cmp-cr s d))
		((eql c 'r) (vp-cmp-rr s d))
		(t (error "No such compare mode" c)))
	(cond
		((eql o '==) (vp-bne l))
		((eql o '!=) (vp-beq l))
		((eql o '<) (vp-bge l))
		((eql o '>) (vp-ble l))
		((eql o '<=) (vp-bgt l))
		((eql o '>=) (vp-blt l))
		(t (error "No such comparator" o))))

(defcfun vp-beq (l) (emit `(emit-beq ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bne (l) (emit `(emit-bne ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-blt (l) (emit `(emit-blt ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-ble (l) (emit `(emit-ble ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bgt (l) (emit `(emit-bgt ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-bge (l) (emit `(emit-bge ,(label-sym l) ,(dec (length (push *distance* 0))))))

(defcfun vp-push (&rest b) (emit `(emit-push ~b)))
(defcfun vp-pop (&rest b) (emit `(emit-pop ~b)))

(defcfun vp-call (l) (emit `(emit-call ,(label-sym l))))
(defcfun vp-call-r (&rest b) (emit `(emit-call-r ~b)))
(defcfun vp-call-i (&rest b) (emit `(emit-call-i ~b)))
(defcfun vp-call-d (&rest b) (emit `(emit-call-d ~b)))
(defcfun vp-call-p (l) (emit `(emit-call-p ,(label-sym l))))

(defcfun vp-jmp (l) (emit `(emit-jmp ,(label-sym l) ,(dec (length (push *distance* 0))))))
(defcfun vp-jmp-r (&rest b) (emit `(emit-jmp-r ~b)))
(defcfun vp-jmp-i (&rest b) (emit `(emit-jmp-i ~b)))
(defcfun vp-jmp-d (&rest b) (emit `(emit-jmp-d ~b)))
(defcfun vp-jmp-p (l) (emit `(emit-jmp-p ,(label-sym l))))

(defcfun vp-lea-i (&rest b) (emit `(emit-lea-i ~b)))
(defcfun vp-lea-d (&rest b) (emit `(emit-lea-d ~b)))
(defcfun vp-lea-p (l r) (emit `(emit-lea-p ,(label-sym l) ,r)))

(defcfun vp-cpy-cr (&rest b) (emit `(emit-cpy-cr ~b)))
(defcfun vp-cpy-rr (&rest b) (emit `(emit-cpy-rr ~b)))
(defcfun vp-cpy-ir (&rest b) (emit `(emit-cpy-ir ~b)))
(defcfun vp-cpy-dr (&rest b) (emit `(emit-cpy-dr ~b)))
(defcfun vp-cpy-pr (l r) (emit `(emit-cpy-pr ,(label-sym l) ,r)))
(defcfun vp-cpy-ri (&rest b) (emit `(emit-cpy-ri ~b)))
(defcfun vp-cpy-rd (&rest b) (emit `(emit-cpy-rd ~b)))
(defcfun vp-cpy-rp (r l) (emit `(emit-cpy-rp ,r ,(label-sym l))))

(defcfun vp-cpy-ir-b (&rest b) (emit `(emit-cpy-ir-b ~b)))
(defcfun vp-cpy-dr-b (&rest b) (emit `(emit-cpy-dr-b ~b)))
(defcfun vp-cpy-ir-ub (&rest b) (emit `(emit-cpy-ir-ub ~b)))
(defcfun vp-cpy-dr-ub (&rest b) (emit `(emit-cpy-dr-ub ~b)))
(defcfun vp-cpy-ir-s (&rest b) (emit `(emit-cpy-ir-s ~b)))
(defcfun vp-cpy-dr-s (&rest b) (emit `(emit-cpy-dr-s ~b)))
(defcfun vp-cpy-ir-us (&rest b) (emit `(emit-cpy-ir-us ~b)))
(defcfun vp-cpy-dr-us (&rest b) (emit `(emit-cpy-dr-us ~b)))
(defcfun vp-cpy-ir-i (&rest b) (emit `(emit-cpy-ir-i ~b)))
(defcfun vp-cpy-dr-i (&rest b) (emit `(emit-cpy-dr-i ~b)))
(defcfun vp-cpy-ir-ui (&rest b) (emit `(emit-cpy-ir-ui ~b)))
(defcfun vp-cpy-dr-ui (&rest b) (emit `(emit-cpy-dr-ui ~b)))

(defcfun vp-cpy-ri-b (&rest b) (emit `(emit-cpy-ri-b ~b)))
(defcfun vp-cpy-rd-b (&rest b) (emit `(emit-cpy-rd-b ~b)))
(defcfun vp-cpy-ri-s (&rest b) (emit `(emit-cpy-ri-s ~b)))
(defcfun vp-cpy-rd-s (&rest b) (emit `(emit-cpy-rd-s ~b)))
(defcfun vp-cpy-ri-i (&rest b) (emit `(emit-cpy-ri-i ~b)))
(defcfun vp-cpy-rd-i (&rest b) (emit `(emit-cpy-rd-i ~b)))

(defcfun vp-add-cr (&rest b) (emit `(emit-add-cr ~b)))
(defcfun vp-add-rr (&rest b) (emit `(emit-add-rr ~b)))
(defcfun vp-sub-cr (&rest b) (emit `(emit-sub-cr ~b)))
(defcfun vp-sub-rr (&rest b) (emit `(emit-sub-rr ~b)))
(defcfun vp-cmp-cr (&rest b) (emit `(emit-cmp-cr ~b)))
(defcfun vp-cmp-rr (&rest b) (emit `(emit-cmp-rr ~b)))
(defcfun vp-mul-cr (&rest b) (emit `(emit-mul-cr ~b)))
(defcfun vp-mul-rr (&rest b) (emit `(emit-mul-rr ~b)))
(defcfun vp-and-cr (&rest b) (emit `(emit-and-cr ~b)))
(defcfun vp-and-rr (&rest b) (emit `(emit-and-rr ~b)))
(defcfun vp-or-cr (&rest b) (emit `(emit-or-cr ~b)))
(defcfun vp-or-rr (&rest b) (emit `(emit-or-rr ~b)))
(defcfun vp-xor-cr (&rest b) (emit `(emit-xor-cr ~b)))
(defcfun vp-xor-rr (&rest b) (emit `(emit-xor-rr ~b)))

(defcfun vp-shl-cr (&rest b) (emit `(emit-shl-cr ~b)))
(defcfun vp-shl-rr (&rest b) (emit `(emit-shl-rr ~b)))
(defcfun vp-shr-cr (&rest b) (emit `(emit-shr-cr ~b)))
(defcfun vp-shr-rr (&rest b) (emit `(emit-shr-rr ~b)))
(defcfun vp-asr-cr (&rest b) (emit `(emit-asr-cr ~b)))
(defcfun vp-asr-rr (&rest b) (emit `(emit-asr-rr ~b)))

(defcfun vp-ret () (emit '(emit-ret)))
(defcfun vp-inc (r) (emit `(emit-inc ,r)))
(defcfun vp-dec (r) (emit `(emit-dec ,r)))
(defcfun vp-xchg-rr (&rest b) (emit `(emit-xchg-rr ~b)))
(defcfun vp-ext-rr (s d) (emit `(emit-ext-rr ,s ,d ,(dec (length (push *distance* 0))))))
(defcfun vp-div-rrr (&rest b) (emit `(emit-div-rrr ~b)))
(defcfun vp-div-rrr-u (&rest b) (emit `(emit-div-rrr-u ~b)))
(defcfun vp-syscall () (emit `(emit-syscall)))

;;;;;;;;;;;;;
; Emit Buffer
;;;;;;;;;;;;;

(defcvar '*emit-buffer* nil '*out-buffer* (cat "") '*old-out-buffer* (cat ""))

(defcfun emit (f)
	(push *emit-buffer* f))

(defcfun emit-passes ()
	(defq *pass* 0 *pc* 0 *out-stream* (string-stream *out-buffer*) b nil)
	(eval *emit-buffer*)
	(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))
	(until (eql *out-buffer* *old-out-buffer*)
		(setq b *old-out-buffer* *old-out-buffer* *out-buffer* *out-stream* (string-stream b) *pc* 0)
		(eval *emit-buffer*)
		(setq *out-buffer* (str *out-stream*) *pass* (inc *pass*))))

(defcfun print-emit-buffer ()
	(defq i 0)
	(while (lt i (length *emit-buffer*))
		(print i " -> " (elem i *emit-buffer*))
		(setq i (inc i))))

(defcfun emit-label (s)
	(set s *pc*))

(defcfun emit-byte (a &optional b c d e f g h)
	(write-char (defq n 1 s *out-stream*) a)
	(when b (write-char s b)
		(defq n 2)
		(when c (write-char s c)
			(defq n 3)
			(when d (write-char s d)
				(defq n 4)
				(when e (write-char s e)
					(defq n 5)
					(when f (write-char s f)
						(defq n 6)
						(when g (write-char s g)
							(defq n 7)
							(when h (write-char s h)
								(defq n 8))))))))
	(setq *pc* (add *pc* n)))

(defcfun emit-short (&rest b)
	(each (lambda (x)
		(write-char *out-stream* x 2)) b)
	(setq *pc* (add *pc* (mul (length b) 2))))

(defcfun emit-int (&rest b)
	(each (lambda (x)
		(write-char *out-stream* x 4)) b)
	(setq *pc* (add *pc* (mul (length b) 4))))

(defcfun emit-long (&rest b)
	(each (lambda (x)
		(write-char *out-stream* x 8)) b)
	(setq *pc* (add *pc* (mul (length b) 8))))

(defcfun emit-string (s)
	(write *out-stream* s)
	(setq *pc* (add *pc* (length s))))

(defcfun emit-align (a &optional b)
	(when (ne *pass* 0)
		(defq p *pc* b (if b (bit-and b 0xff) 0) s *out-stream*)
		(setq *pc* (align p a))
		(while (le (setq p (inc p)) *pc*)
			(write-char s b))))

;;;;;;;;;;;;;;;;;;;;;
; Arch Emit Functions
;;;;;;;;;;;;;;;;;;;;;

(cond
((eql *cpu* 'x86_64)
(import 'inc/x64.inc))
((eql *cpu* 'ARM)
(import 'inc/arm.inc))
(t (error "No such CPU !" *cpu*)))
