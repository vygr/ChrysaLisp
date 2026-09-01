;;;;;;;;;;;;;;;;;;;;
; x64 Emit Functions
;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;cpu specific requirements
;no registers need saving !
(defcvar 'stack_align 8 'stack_state '())

(defq +emit_native_regs
	(reduce (# (pinsert %0 %1 (num (!))))
		'(:r0 :r1 :r2 :r3 :rsp :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14)
	(reduce (# (pinsert %0 %1 (num (!)))) +vp_fregs (cat +vp_regs_map))))

(defmacro emit-native-reg? (r) (static-qq (pfind +emit_native_regs ,r)))

(defq +x64_rax (emit-native-reg? :r0)
	+x64_rcx (emit-native-reg? :r1)
	+x64_rdx (emit-native-reg? :r2)
	+x64_rsp (emit-native-reg? :rsp)
	+x64_lookback_window 6)

;;;;;;;;;;;;;;;;;;;;;;;;
; Core Encoding Macros
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro x64-within (c n)
	(defq limit (<< 1 (dec n)))
	`(<= ,(neg limit) ,c ,(dec limit)))

(defmacro x64-rex (w r x b) `(+ 0x40 (<< ,w 3) (<< ,r 2) (<< ,x 1) ,b))
(defmacro x64-modrm (mod reg rm) `(+ (<< ,mod 6) (<< (logand 7 ,reg) 3) (logand 7 ,rm)))
(defmacro x64-sib (scale index base) `(+ (<< ,scale 6) (<< (logand 7 ,index) 3) (logand 7 ,base)))

(defmacro x64-r-r (o s d) `(emitm-byte (x64-rex 1 (>> ,s 3) 0 (>> ,d 3)) ,o (x64-modrm 3 ,s ,d)))
(defmacro x64-0f-r-r (o s d) `(emitm-byte (x64-rex 1 (>> ,d 3) 0 (>> ,s 3)) 0x0f ,o (x64-modrm 3 ,d ,s)))
(defmacro x64-xmm-rr (p o s d) `(emitm-byte ,p (x64-rex 0 (>> ,d 3) 0 (>> ,s 3)) 0x0f ,o (x64-modrm 3 ,d ,s)))
(defmacro x64-sse-ff (o s d) `(x64-xmm-rr 0xf2 ,o ,s ,d))
(defmacro x64-gpr-xmm-rr (p o s d) `(emitm-byte ,p (x64-rex 1 (>> ,d 3) 0 (>> ,s 3)) 0x0f ,o (x64-modrm 3 ,d ,s)))

(defmacro x64-s-cond (cc d)
	`(progn
		(if (>= ,d 4) (emitm-byte (x64-rex 0 0 0 (>> ,d 3))))
		(emitm-byte 0x0f (+ 0x20 ,cc) (x64-modrm 3 0 ,d))
		(if (>= ,d 4) (emitm-byte (x64-rex 0 (>> ,d 3) 0 (>> ,d 3))))
		(emitm-byte 0x0f 0xb6 (x64-modrm 3 ,d ,d))
		(emitm-byte (x64-rex 1 0 0 (>> ,d 3)) 0xf7 (x64-modrm 3 3 ,d))))

(defun x64-mem-ir (w o reg base disp &optional pfx)
	(defq rh (>> reg 3) bh (>> base 3))
	(if pfx (emitm-byte pfx))
	(cond
		((= w 8)
			(emitm-byte (x64-rex 1 rh 0 bh)))
		((= w 1)
			(if (or (>= reg 4) (>= base 8))
				(emitm-byte (x64-rex 0 rh 0 bh))))
		((or (>= reg 8) (>= base 8))
			(emitm-byte (x64-rex 0 rh 0 bh))))
	(if (list?? o)
		(emitm-byte (first o) (second o))
		(emitm-byte o))
	(defq need_sib (or (= base 4) (= base 12)))
	(cond
		((and (eql disp 0) (/= base 5) (/= base 13))
			(emitm-byte (x64-modrm 0 reg base))
			(if need_sib (emitm-byte 0x24)))
		((x64-within disp 8)
			(emitm-byte (x64-modrm 1 reg base))
			(if need_sib (emitm-byte 0x24))
			(emitm-byte disp))
		((x64-within disp 32)
			(emitm-byte (x64-modrm 2 reg base))
			(if need_sib (emitm-byte 0x24))
			(emitm-int disp))
		(:t (throw "x64-mem-ir constant out of range !" disp))))

(defun x64-mem-dr (w o reg base index &optional pfx)
	(defq rh (>> reg 3) bh (>> base 3) ih (>> index 3))
	(if pfx (emitm-byte pfx))
	(cond
		((= w 8)
			(emitm-byte (x64-rex 1 rh ih bh)))
		((= w 1)
			(if (or (>= reg 4) (>= base 8) (>= index 8))
				(emitm-byte (x64-rex 0 rh ih bh))))
		((or (>= reg 8) (>= base 8) (>= index 8))
			(emitm-byte (x64-rex 0 rh ih bh))))
	(if (list?? o)
		(emitm-byte (first o) (second o))
		(emitm-byte o))
	(cond
		((and (/= base 5) (/= base 13))
			(emitm-byte (x64-modrm 0 reg 4) (x64-sib 0 index base)))
		(:t (emitm-byte (x64-modrm 1 reg 4) (x64-sib 0 index base) 0))))

(defun x64-pr (o l d)
	(defq l (- l *pc* 7))
	(cond
		((x64-within l 32)
			(emitm-byte (x64-rex 1 (>> d 3) 0 0) o (x64-modrm 0 d 5))
			(emitm-int l))
		(:t (throw "x64-pr constant out of range !" l))))

(defun x64-alu-cr (digit c r)
	(emitm-byte (x64-rex 1 0 0 (>> r 3)))
	(cond
		((x64-within c 8)
			(emitm-byte 0x83 (x64-modrm 3 digit r) c))
		((x64-within c 32)
			(if (= r +x64_rax)
				(emitm-byte (+ (<< digit 3) 5))
				(emitm-byte 0x81 (x64-modrm 3 digit r)))
			(emitm-int c))
		(:t (throw "x64-alu-cr constant out of range !" c))))

(defun x64-shift-cr (digit c r)
	(unless (= c 0)
		(emitm-byte (x64-rex 1 0 0 (>> r 3)))
		(cond
			((= c 1)
				(emitm-byte 0xd1 (x64-modrm 3 digit r)))
			((<= c 0xff)
				(emitm-byte 0xc1 (x64-modrm 3 digit r) c))
			(:t (throw "x64-shift-cr constant out of range !" c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Low-Level Stack & Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-push (&rest b)
	(each (lambda (r)
		(if (>= r 8)
			(emitm-byte (x64-rex 0 0 0 1) (+ 0x50 (logand 7 r)))
			(emitm-byte (+ 0x50 r)))) b))

(defun emit-pop (&rest b)
	(reach (lambda (r)
		(if (>= r 8)
			(emitm-byte (x64-rex 0 0 0 1) (+ 0x58 (logand 7 r)))
			(emitm-byte (+ 0x58 r)))) b))

(defun emit-ret () (emitm-byte 0xc3))
(defun emit-sync (n) (emitm-byte 0x0f 0xae 0xf0))
(defun emit-brk (n))

;;;;;;;;;;;;;;;;;;;;;;
; Group 1: ALU & Moves
;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-rr (s d) (unless (eql s d) (x64-r-r 0x89 s d)))
(defun emit-add-rr (s d) (x64-r-r 0x01 s d))
(defun emit-sub-rr (s d) (x64-r-r 0x29 s d))
(defun emit-and-rr (s d) (unless (eql s d) (x64-r-r 0x21 s d)))
(defun emit-or-rr (s d) (unless (eql s d) (x64-r-r 0x09 s d)))
(defun emit-or-cr (c r) (unless (= c 0) (x64-alu-cr 1 c r)))
(defun emit-xor-cr (c r) (unless (= c 0) (x64-alu-cr 6 c r)))
(defun emit-xor-rr (s d)
	(if (eql s d)
		(progn
			(if (>= d 8) (emitm-byte 0x45))
			(emitm-byte 0x31 (x64-modrm 3 d d)))
		(x64-r-r 0x31 s d)))

(defun emit-cpy-cr (c r)
	(if (= c 0)
		(emit-xor-rr r r)
		(progn
			(defq rl (logand r 7) rh (>> r 3))
			(cond
				((= -1 c)
					(emit-or-cr -1 r))
				((<= 0 c 0xffffffff)
					(if (>= r 8) (emitm-byte (x64-rex 0 0 0 rh)))
					(emitm-byte (+ 0xb8 rl))
					(emitm-int c))
				((x64-within c 32)
					(emitm-byte (x64-rex 1 0 0 rh) 0xc7 (x64-modrm 3 0 r))
					(emitm-int c))
				(:t (emitm-byte (x64-rex 1 0 0 rh) (+ 0xb8 rl))
					(emitm-long c))))))

(defun emit-add-cr (c r)
	(unless (= c 0)
		(cond
			((= c 1)
				(emitm-byte (x64-rex 1 0 0 (>> r 3)) 0xff (x64-modrm 3 0 r)))
			((= c -1)
				(emitm-byte (x64-rex 1 0 0 (>> r 3)) 0xff (x64-modrm 3 1 r)))
			(:t (x64-alu-cr 0 c r)))))

(defun emit-sub-cr (c r)
	(unless (= c 0)
		(cond
			((= c 1)
				(emitm-byte (x64-rex 1 0 0 (>> r 3)) 0xff (x64-modrm 3 1 r)))
			((= c -1)
				(emitm-byte (x64-rex 1 0 0 (>> r 3)) 0xff (x64-modrm 3 0 r)))
			(:t (x64-alu-cr 5 c r)))))

(defun emit-and-cr (c r)
	(unless (= c -1)
		(if (= c 0)
			(emit-xor-rr r r)
			(x64-alu-cr 4 c r))))

(defun x64-cmp-rr (s d) (x64-r-r 0x39 s d))
(defun x64-cmp-cr (c r) (if (= c 0) (x64-r-r 0x85 r r) (x64-alu-cr 7 c r)))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) +x64_rsp))
(defun emit-free (c) (emit-add-cr (align c stack_align) +x64_rsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 2: Memory Load & Store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-ir (s c d) (x64-mem-ir 8 0x8b d s c))
(defun emit-cpy-ir-b (s c d) (x64-mem-ir 8 '(0x0f 0xbe) d s c))
(defun emit-cpy-ir-s (s c d) (x64-mem-ir 8 '(0x0f 0xbf) d s c))
(defun emit-cpy-ir-i (s c d) (x64-mem-ir 8 0x63 d s c))
(defun emit-cpy-ir-ub (s c d) (x64-mem-ir 4 '(0x0f 0xb6) d s c))
(defun emit-cpy-ir-us (s c d) (x64-mem-ir 4 '(0x0f 0xb7) d s c))
(defun emit-cpy-ir-ui (s c d) (x64-mem-ir 4 0x8b d s c))
(defun emit-lea-i (s c d) (unless (and (eql c 0) (eql s d)) (x64-mem-ir 8 0x8d d s c)))

(defun emit-cpy-ri (s d c) (x64-mem-ir 8 0x89 s d c))
(defun emit-cpy-ri-b (s d c) (x64-mem-ir 1 0x88 s d c))
(defun emit-cpy-ri-s (s d c) (x64-mem-ir 2 0x89 s d c 0x66))
(defun emit-cpy-ri-i (s d c) (x64-mem-ir 4 0x89 s d c))

(defun emit-cpy-dr (s1 s2 d) (x64-mem-dr 8 0x8b d s1 s2))
(defun emit-cpy-dr-b (s1 s2 d) (x64-mem-dr 8 '(0x0f 0xbe) d s1 s2))
(defun emit-cpy-dr-s (s1 s2 d) (x64-mem-dr 8 '(0x0f 0xbf) d s1 s2))
(defun emit-cpy-dr-i (s1 s2 d) (x64-mem-dr 8 0x63 d s1 s2))
(defun emit-cpy-dr-ub (s1 s2 d) (x64-mem-dr 4 '(0x0f 0xb6) d s1 s2))
(defun emit-cpy-dr-us (s1 s2 d) (x64-mem-dr 4 '(0x0f 0xb7) d s1 s2))
(defun emit-cpy-dr-ui (s1 s2 d) (x64-mem-dr 4 0x8b d s1 s2))
(defun emit-lea-d (s1 s2 d) (x64-mem-dr 8 0x8d d s1 s2))

(defun emit-cpy-rd (s d1 d2) (x64-mem-dr 8 0x89 s d1 d2))
(defun emit-cpy-rd-b (s d1 d2) (x64-mem-dr 1 0x88 s d1 d2))
(defun emit-cpy-rd-s (s d1 d2) (x64-mem-dr 2 0x89 s d1 d2 0x66))
(defun emit-cpy-rd-i (s d1 d2) (x64-mem-dr 4 0x89 s d1 d2))

(defun emit-cpy-pr (l d) (x64-pr 0x8b l d))
(defun emit-lea-p (l d) (x64-pr 0x8d l d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 3: Swaps, Shifts, Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(if (eql s +x64_rax) (setq s d d +x64_rax))
		(defq sl (logand 7 s) sh (>> s 3) dl (logand 7 d) dh (>> d 3))
		(if (= d +x64_rax)
			(emitm-byte (x64-rex 1 0 0 sh) (+ 0x90 sl))
			(emitm-byte (x64-rex 1 sh 0 dh) 0x87 (x64-modrm 3 s d)))))

(defun x64-shift-rr (digit s d)
	(cond
		((eql s +x64_rcx)
			(emitm-byte (x64-rex 1 0 0 (>> d 3)) 0xd3 (x64-modrm 3 digit d)))
		((eql d +x64_rcx)
			(emit-swp-rr s d)
			(emitm-byte (x64-rex 1 0 0 (>> s 3)) 0xd3 (x64-modrm 3 digit s))
			(emit-swp-rr s d))
		(:t (emit-push d s +x64_rax +x64_rcx)
			(emit-cpy-ir +x64_rsp 24 +x64_rax)
			(emit-cpy-ir +x64_rsp 16 +x64_rcx)
			(emitm-byte (x64-rex 1 0 0 0) 0xd3 (x64-modrm 3 digit 0))
			(emit-cpy-ri +x64_rax +x64_rsp 24)
			(emit-pop d s +x64_rax +x64_rcx))))

(defun emit-shl-cr (c r) (x64-shift-cr 4 c r))
(defun emit-shr-cr (c r) (x64-shift-cr 5 c r))
(defun emit-asr-cr (c r) (x64-shift-cr 7 c r))

(defun emit-shl-rr (s d) (x64-shift-rr 4 s d))
(defun emit-shr-rr (s d) (x64-shift-rr 5 s d))
(defun emit-asr-rr (s d) (x64-shift-rr 7 s d))

(defun emit-lnot-rr (r d)
	(if (>= d 4)
		(throw "emit-lnot-rr illegal register !" d))
	(emit-cpy-rr d r)
	(emit-xor-rr d d)
	(x64-cmp-cr 0 r)
	(emitm-byte 0x0f 0x94 (x64-modrm 3 0 d)))

(defun emit-land-rr (s d)
	(if (or (>= s 4) (>= d 4))
		(throw "emit-land-rr illegal register !" (list s d)))
	(x64-cmp-cr 0 d)
	(emitm-byte 0x0f 0x95 (x64-modrm 3 0 d))
	(x64-cmp-cr 0 s)
	(emitm-byte 0x0f 0x95 (x64-modrm 3 0 s))
	(emit-and-rr s d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 4: Multiply, Divide, Ext
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-ext-rr (s d)
	(cond
		((and (eql s +x64_rax) (eql d +x64_rdx))
			(emitm-byte 0x48 0x99))
		((eql s d)
			(emit-asr-cr 63 d))
		(:t (emit-cpy-rr s d)
			(emit-asr-cr 63 d))))

(defun emit-mul-rr (s d) (x64-0f-r-r 0xaf s d))

(defun emit-mul-cr (c r)
	(cond
		((= c 1))
		((= c 0)
			(emit-xor-rr r r))
		((defq s (log2 c))
			(emit-shl-cr s r))
		(:t (defq rl (logand r 7) rh (>> r 3))
			(cond
				((= -1 c)
					(emitm-byte (x64-rex 1 0 0 rh) 0xf7 (x64-modrm 3 3 r)))
				((x64-within c 8)
					(emitm-byte (x64-rex 1 rh 0 rh) 0x6b (x64-modrm 3 r r) c))
				((x64-within c 32)
					(emitm-byte (x64-rex 1 rh 0 rh) 0x69 (x64-modrm 3 r r))
					(emitm-int c))
				(:t (throw "emit-mul-cr constant out of range !" c))))))

(defun emit-div-rrr (s d1 d2)
	(cond
		((and (eql d1 +x64_rdx) (eql d2 +x64_rax))
			(emitm-byte (x64-rex 1 0 0 (>> s 3)) 0xf7 (x64-modrm 3 7 s)))
		((and (eql d1 +x64_rax) (eql d2 +x64_rdx) (nql s +x64_rax) (nql s +x64_rdx))
			(emitm-byte (x64-rex 1 0 0 0) 0x92 (x64-rex 1 0 0 (>> s 3)) 0xf7 (x64-modrm 3 7 s) (x64-rex 1 0 0 0) 0x92))
		(:t (emit-push d2 d1 s +x64_rdx +x64_rcx +x64_rax)
			(emit-cpy-ir +x64_rsp 24 +x64_rcx)
			(emit-cpy-ir +x64_rsp 32 +x64_rdx)
			(emit-cpy-ir +x64_rsp 40 +x64_rax)
			(emitm-byte (x64-rex 1 0 0 0) 0xf7 (x64-modrm 3 7 1))
			(emit-cpy-ri +x64_rdx +x64_rsp 32)
			(emit-cpy-ri +x64_rax +x64_rsp 40)
			(emit-pop d2 d1 s +x64_rdx +x64_rcx +x64_rax))))

(defun emit-div-rrr-u (s d1 d2)
	(cond
		((and (eql d1 +x64_rdx) (eql d2 +x64_rax))
			(emitm-byte (x64-rex 1 0 0 (>> s 3)) 0xf7 (x64-modrm 3 6 s)))
		((and (eql d1 +x64_rax) (eql d2 +x64_rdx) (nql s +x64_rax) (nql s +x64_rdx))
			(emitm-byte (x64-rex 1 0 0 0) 0x92 (x64-rex 1 0 0 (>> s 3)) 0xf7 (x64-modrm 3 6 s) (x64-rex 1 0 0 0) 0x92))
		(:t (emit-push d2 d1 s +x64_rdx +x64_rcx +x64_rax)
			(emit-cpy-ir +x64_rsp 24 +x64_rcx)
			(emit-cpy-ir +x64_rsp 32 +x64_rdx)
			(emit-cpy-ir +x64_rsp 40 +x64_rax)
			(emitm-byte (x64-rex 1 0 0 0) 0xf7 (x64-modrm 3 6 1))
			(emit-cpy-ri +x64_rdx +x64_rsp 32)
			(emit-cpy-ri +x64_rax +x64_rsp 40)
			(emit-pop d2 d1 s +x64_rdx +x64_rcx +x64_rax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 5: Branches & Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x64-branch (o l d)
	(defq m (elem-get *offsets* d) l (- l *pc*))
	(and (> *pass* 0) (> (abs l) (abs m)) (elem-set *offsets* d (setq m l)))
	(cond
		((x64-within (- m 2) 8)
			(emitm-byte o (- l 2)))
		((x64-within (- m 6) 32)
			(emitm-byte 0x0f (+ 0x10 o))
			(emitm-int (- l 6)))
		(:t (throw "x64-branch constant out of range !" l))))

(defun emit-beq-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x74 l m))
(defun emit-bne-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x75 l m))
(defun emit-bge-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x7d l m))
(defun emit-blt-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x7c l m))
(defun emit-ble-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x7e l m))
(defun emit-bgt-cr (c d l m) (x64-cmp-cr c d) (x64-branch 0x7f l m))

(defun emit-beq-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x74 l m))
(defun emit-bne-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x75 l m))
(defun emit-bge-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x7d l m))
(defun emit-blt-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x7c l m))
(defun emit-ble-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x7e l m))
(defun emit-bgt-rr (s d l m) (x64-cmp-rr s d) (x64-branch 0x7f l m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 6: Comparisons & Sxx Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-seq-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x74 d))
(defun emit-sne-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x75 d))
(defun emit-slt-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x7c d))
(defun emit-sle-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x7e d))
(defun emit-sgt-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x7f d))
(defun emit-sge-cr (c d) (x64-cmp-cr c d) (x64-s-cond 0x7d d))

(defun emit-seq-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x74 d))
(defun emit-sne-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x75 d))
(defun emit-slt-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x7c d))
(defun emit-sle-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x7e d))
(defun emit-sgt-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x7f d))
(defun emit-sge-rr (s d) (x64-cmp-rr s d) (x64-s-cond 0x7d d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 7: Min, Max, Abs, CMOV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-min-cr (c d l m) (emit-ble-cr c d l m) (emit-cpy-cr c d))
(defun emit-max-cr (c d l m) (emit-bge-cr c d l m) (emit-cpy-cr c d))
(defun emit-min-rr (s d l m) (unless (eql s d) (x64-cmp-rr s d) (x64-0f-r-r 0x4f s d)))
(defun emit-max-rr (s d l m) (unless (eql s d) (x64-cmp-rr s d) (x64-0f-r-r 0x4c s d)))

(defun emit-abs-rr (s d l m)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-bge-cr 0 d l m)
	(emit-mul-cr -1 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 8: Calls and Jumps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-call (l)
	(cond
		((x64-within (defq c (- l *pc* 5)) 32)
			(emitm-byte 0xe8)
			(emitm-int c))
		(:t (throw "emit-call constant out of range !" c))))

(defun emit-jmp (l d)
	(defq m (elem-get *offsets* d) c (- l *pc*))
	(and (> *pass* 0) (> (abs c) (abs m)) (elem-set *offsets* d (setq m c)))
	(cond
		((x64-within (- m 2) 8)
			(emitm-byte 0xeb (- c 2)))
		((x64-within (- m 5) 32)
			(emitm-byte 0xe9)
			(emitm-int (- c 5)))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-call-r (r &ignore) (if (>= r 8) (emitm-byte (x64-rex 0 0 0 1))) (emitm-byte 0xff (x64-modrm 3 2 r)))
(defun emit-jmp-r (r &ignore) (if (>= r 8) (emitm-byte (x64-rex 0 0 0 1))) (emitm-byte 0xff (x64-modrm 3 4 r)))
(defun emit-call-i (b c &ignore) (x64-mem-ir 0 0xff 2 b c))
(defun emit-jmp-i (b c &ignore) (x64-mem-ir 0 0xff 4 b c))
(defun emit-call-p (l) (emitm-byte 0xff (x64-modrm 0 2 5)) (emitm-int (- l *pc* 4)))
(defun emit-jmp-p (l) (emitm-byte 0xff (x64-modrm 0 4 5)) (emitm-int (- l *pc* 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 9: Floating Point (SSE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x64-shift-xmm (d digit i)
	(emitm-byte 0x66)
	(if (>= d 8) (emitm-byte (x64-rex 0 0 0 (>> d 3))))
	(emitm-byte 0x0f 0x73 (x64-modrm 3 digit d) i))

(defun emit-cpy-ff (s d) (unless (eql s d) (x64-sse-ff 0x10 s d)))
(defun emit-cpy-rf (s d) (x64-gpr-xmm-rr 0x66 0x6e s d))
(defun emit-cpy-fr (s d) (x64-gpr-xmm-rr 0x66 0x7e d s))
(defun emit-cvt-rf (s d) (x64-gpr-xmm-rr 0xf2 0x2a s d))
(defun emit-cvt-fr (s d) (x64-gpr-xmm-rr 0xf2 0x2c s d))
(defun emit-add-ff (s d) (x64-sse-ff 0x58 s d))
(defun emit-sub-ff (s d) (x64-sse-ff 0x5c s d))
(defun emit-mul-ff (s d) (x64-sse-ff 0x59 s d))
(defun emit-div-ff (s d) (x64-sse-ff 0x5e s d))
(defun emit-min-ff (s d) (x64-sse-ff 0x5d s d))
(defun emit-max-ff (s d) (x64-sse-ff 0x5f s d))
(defun emit-sqrt-ff (s d) (x64-sse-ff 0x51 s d))

(defun emit-abs-ff (s d)
	(unless (eql s d) (emit-cpy-ff s d))
	(x64-shift-xmm d 6 1)
	(x64-shift-xmm d 2 1))

(defun emit-neg-ff (s d)
	(emit-push +x64_rax)
	(emit-cpy-fr s +x64_rax)
	(emitm-byte (x64-rex 1 0 0 0) 0x0f 0xba (x64-modrm 3 7 +x64_rax) 63)
	(emit-cpy-rf +x64_rax d)
	(emit-pop +x64_rax))

(defun x64-cmp-ff (s d) (x64-xmm-rr 0x66 0x2e s d))
(defun emit-beq-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x74 l m))
(defun emit-bne-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x75 l m))
(defun emit-blt-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x72 l m))
(defun emit-bgt-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x77 l m))
(defun emit-ble-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x76 l m))
(defun emit-bge-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x73 l m))

(defun emit-cpy-if (s c d) (x64-mem-ir 8 '(0x0f 0x10) d s c 0xf2))
(defun emit-cpy-fi (s d c) (x64-mem-ir 8 '(0x0f 0x11) s d c 0xf2))

(defun x64-sse-mem-dr (o reg s1 s2)
	(if (or (= s2 4) (= s2 12))
		(defq tmp s1 s1 s2 s2 tmp))
	(x64-mem-dr 0 o reg s1 s2 0xf2))

(defun emit-cpy-df (s1 s2 d) (x64-sse-mem-dr '(0x0f 0x10) d s1 s2))
(defun emit-cpy-fd (s d1 d2) (x64-sse-mem-dr '(0x0f 0x11) s d1 d2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 10: Stack & ABI Linkage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-sub-cr (+ tk_state_size (* +ptr_size 2)) s)
	(emit-cpy-ri f s tk_state_size)
	(emit-cpy-ri x s (+ tk_state_size +ptr_size)))

(case *abi*
(AMD64
(defun emit-call-abi (r b c n &rest x)
	(cond
		((= *func_align* 16)
			(apply (const emit-push) x)
			(emit-call-i b c)
			(emit-add-cr (* +ptr_size (length x)) +x64_rsp))
		(:t (setq x (cat (list r r) x))
			(emit-cpy-rr +x64_rsp r)
			(emit-and-cr -16 +x64_rsp)
			(apply (const emit-push) x)
			(emit-call-i b c)
			(emit-cpy-ir +x64_rsp (* +ptr_size (- (length x) 2)) +x64_rsp)))))
(WIN64
(defun emit-call-abi (r b c n &rest x)
	(cond
		((= *func_align* 16)
			(if (odd? (length x))
				(setq x (cat (list r) x)))
			(apply (const emit-push) x)
			(emit-sub-cr 32 +x64_rsp)
			(emit-call-i b c)
			(emit-add-cr (* +ptr_size (+ (length x) 4)) +x64_rsp))
		(:t (emit-cpy-rr +x64_rsp r)
			(emit-and-cr -16 +x64_rsp)
			(if (odd? (length x))
				(defq y (cat (list r) x))
				(defq y (cat (list r r) x)))
			(apply (const emit-push) y)
			(emit-sub-cr 32 +x64_rsp)
			(emit-call-i b c)
			(emit-cpy-ir +x64_rsp (* +ptr_size (+ (length x) 4)) +x64_rsp)))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; x86_64 Specific Emitters
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-x64-alu-ir (o d b c) (x64-mem-ir 8 o d b c))

(defq emit_list (filter (# (starts-with "emit-" %0))
		(merge (map (const first) (tolist (penv))) (map (const first) (tolist (env)))))
	+x64_cpy_fuse_map
		(reduce (#
			(pinsert %0 %1
				(cond
					((eql %1 'emit-add-rr)
						(list :rr 'emit-lea-d))
					((eql %1 'emit-add-cr)
						(list :add_cr 'emit-lea-i 1))
					((eql %1 'emit-sub-cr)
						(list :add_cr 'emit-lea-i -1))
					((eql %1 'emit-shl-cr)
						(list :shl_cr 'emit-lea-d)))))
			emit_list (pmap))
	+x64_mem_alu_map
		(reduce (#
			(pinsert %0 %1
				(cond
					((eql %1 'emit-add-rr) 0x03)
					((eql %1 'emit-sub-rr) 0x2b)
					((eql %1 'emit-and-rr) 0x23)
					((eql %1 'emit-or-rr)  0x0b)
					((eql %1 'emit-xor-rr) 0x33)
					((eql %1 'emit-mul-rr) '(0x0f 0xaf)))))
			emit_list (pmap))
	+x64_barrier_map
		(reduce (#
			(pinsert %0 %1
				(when (find %1 '(emit-label emit-tlabel emit-alloc emit-free emit-push emit-pop
						emit-call emit-call-r emit-call-i emit-call-p emit-call-abi
						emit-jmp emit-jmp-r emit-jmp-i emit-jmp-p emit-ret emit-sync emit-brk
						emit-beq-cr emit-bne-cr emit-bge-cr emit-ble-cr emit-blt-cr emit-bgt-cr
						emit-beq-rr emit-bne-rr emit-bge-rr emit-ble-rr emit-blt-rr emit-bgt-rr
						emit-beq-ff emit-bne-ff emit-bge-ff emit-blt-ff emit-ble-ff emit-bgt-ff))
					:t)))
			emit_list (pmap)))

(defun x64-find-cpy (emit_list idx d &optional x)
	(if (eql x d) (setq x :nil))
	(defq res (some! (#
			(defq op (first %0))
			(cond
				((eql op 'emit-nop) :nil)
				((and (eql op 'emit-cpy-rr) (eql (third %0) d))
					(defq s (second %0))
					(if (and (nql s d) (nql s :rsp))
						(list (!) s)
						:fail))
				((or (pfind +x64_barrier_map op)
						(find d %0 1)
						(and x (find x %0 1)))
					:fail)))
			(list emit_list) :nil idx (max 9 (- idx +x64_lookback_window))))
	(if (and res (nql res :fail)) res))

(defun x64-find-load (emit_list idx s d)
	(defq res (some! (#
			(defq op (first %0))
			(cond
				((eql op 'emit-nop) :nil)
				((and (eql op 'emit-cpy-ir) (eql (last %0) s))
					(bind '(& base disp &ignore) %0)
					(if (and (nql base d) (nql s d) (x64-within disp 32))
						(list (!) base disp)
						:fail))
				((or (pfind +x64_barrier_map op)
						(find s %0 1)
						(find d %0 1)
						(starts-with "emit-cpy-r" (str op)))
					:fail)))
			(list emit_list) :nil idx (max 9 (- idx +x64_lookback_window))))
	(if (and res (nql res :fail)) res))

(defun x64-reg-dead? (emit_list idx s)
	(eql (some! (#
			(defq op (first %0))
			(cond
				((eql op 'emit-nop) :nil)
				((find s %0 1)
					(if (and (find op '(emit-cpy-cr emit-cpy-ir emit-cpy-ir-b emit-cpy-ir-s
							emit-cpy-ir-i emit-cpy-ir-ub emit-cpy-ir-us emit-cpy-ir-ui
							emit-lea-i emit-lea-p emit-cpy-pr emit-xor-rr))
							(eql (last %0) s)
							(nql (second %0) s))
						:dead
						:live))
				((pfind +x64_barrier_map op) :live)))
			(list emit_list) :nil (inc idx) (min (length emit_list) (+ idx 16)))
		:dead))

(redefun emit-prepass (emit_list)
	(each! (lambda (%0)
		(defq op0 (first %0))
		(cond
			((and (defq alu_op (pfind +x64_mem_alu_map op0))
					(defq s (second %0) d (third %0))
					(nql s :rsp)
					(nql d :rsp)
					(defq load_match (x64-find-load emit_list (!) s d))
					(x64-reg-dead? emit_list (!) s))
				(bind '(p_idx base disp) load_match)
				(elem-set emit_list p_idx '(emit-nop))
				(elem-set emit_list (!) (list 'emit-x64-alu-ir alu_op d base disp)))
			((and (defq cpy_info (pfind +x64_cpy_fuse_map op0))
					(defq d (third %0))
					(nql d :rsp))
				(case (first cpy_info)
					(:rr
						(and (nql (defq x (second %0)) :rsp)
							(defq match (x64-find-cpy emit_list (!) d x))
							(progn
								(bind '(p_idx s) match)
								(elem-set emit_list p_idx (list (second cpy_info) s (if (eql x d) s x) d))
								(elem-set emit_list (!) '(emit-nop)))))
					(:add_cr
						(and (x64-within (defq c (second %0)) 32)
							(defq match (x64-find-cpy emit_list (!) d))
							(progn
								(bind '(p_idx s) match)
								(elem-set emit_list p_idx (list (second cpy_info) s (* c (third cpy_info)) d))
								(elem-set emit_list (!) '(emit-nop)))))
					(:shl_cr
						(and (eql (second %0) 1)
							(defq match (x64-find-cpy emit_list (!) d))
							(progn
								(bind '(p_idx s) match)
								(elem-set emit_list p_idx (list (second cpy_info) s s d))
								(elem-set emit_list (!) '(emit-nop)))))))))
		(list emit_list) 9 -1)
	(filter! (# (nql (first %0) 'emit-nop)) emit_list))

;module, that exports all emit-* !
(export-symbols emit_list)
;plus any private instruction maps if validating
(validatecase
	(defq emit_maps (list +x64_cpy_fuse_map +x64_mem_alu_map +x64_barrier_map))
	(export-symbols '(emit_maps)))
(env-pop)