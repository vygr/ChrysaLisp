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
	(reduce (# (pinsert %0 %1 (num (!))))
		'(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15)
		(cat +vp_regs_map))))

(defmacro emit-native-reg? (r) (static-qq (pfind +emit_native_regs ,r)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Core Encoding Macros
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro x64-r-r (o s d)
	`(emitm-byte (+ 0x48 (<< (>> ,s 3) 2) (>> ,d 3)) ,o (+ 0xc0 (<< (logand 7 ,s) 3) (logand 7 ,d))))

(defmacro x64-0f-r-r (o s d)
	`(emitm-byte (+ 0x48 (<< (>> ,d 3) 2) (>> ,s 3)) 0x0f ,o (+ 0xc0 (<< (logand 7 ,d) 3) (logand 7 ,s))))

(defmacro x64-xmm-rr (p o s d)
	`(emitm-byte ,p (+ 0x40 (<< (>> ,d 3) 2) (>> ,s 3)) 0x0f ,o (+ 0xc0 (<< (logand 7 ,d) 3) (logand 7 ,s))))

(defmacro x64-gpr-xmm-rr (p o s d)
	`(emitm-byte ,p (+ 0x48 (<< (>> ,d 3) 2) (>> ,s 3)) 0x0f ,o (+ 0xc0 (<< (logand 7 ,d) 3) (logand 7 ,s))))

(defun x64-mem-ir (w pfx o reg base disp)
	(defq rl (logand 7 reg) rh (>> reg 3)
		bl (logand 7 base) bh (>> base 3))
	(if pfx (emitm-byte pfx))
	(cond
		((eql w 1)
			(emitm-byte (+ 0x48 (<< rh 2) bh)))
		((eql w :b)
			(if (or (>= reg 4) (>= base 8))
				(emitm-byte (+ 0x40 (<< rh 2) bh))))
		((or (>= reg 8) (>= base 8))
			(emitm-byte (+ 0x40 (<< rh 2) bh))))
	(if (list?? o)
		(emitm-byte (first o) (second o))
		(emitm-byte o))
	(cond
		((and (eql disp 0) (/= base 5) (/= base 13))
			(emitm-byte (+ (<< rl 3) bl))
			(if (or (= base 4) (= base 12)) (emitm-byte 0x24)))
		((<= -0x80 disp 0x7f)
			(emitm-byte (+ 0x40 (<< rl 3) bl))
			(if (or (= base 4) (= base 12)) (emitm-byte 0x24))
			(emitm-byte disp))
		((<= -0x80000000 disp 0x7fffffff)
			(emitm-byte (+ 0x80 (<< rl 3) bl))
			(if (or (= base 4) (= base 12)) (emitm-byte 0x24))
			(emitm-int disp))
		(:t (throw "x64-mem-ir constant out of range !" disp))))

(defun x64-mem-dr (w pfx o reg base index)
	(defq rl (logand 7 reg) rh (>> reg 3)
		bl (logand 7 base) bh (>> base 3)
		il (logand 7 index) ih (>> index 3))
	(if pfx (emitm-byte pfx))
	(cond
		((eql w 1)
			(emitm-byte (+ 0x48 (<< rh 2) (<< ih 1) bh)))
		((eql w :b)
			(if (or (>= reg 4) (>= base 8) (>= index 8))
				(emitm-byte (+ 0x40 (<< rh 2) (<< ih 1) bh))))
		((or (>= reg 8) (>= base 8) (>= index 8))
			(emitm-byte (+ 0x40 (<< rh 2) (<< ih 1) bh))))
	(if (list?? o)
		(emitm-byte (first o) (second o))
		(emitm-byte o))
	(cond
		((and (/= base 5) (/= base 13))
			(emitm-byte (+ 0x04 (<< rl 3)) (+ (<< il 3) bl)))
		(:t (emitm-byte (+ 0x44 (<< rl 3)) (+ (<< il 3) bl) 0))))

(defun x64-pr (o l d)
	(defq dl (logand 7 d) dh (>> d 3) l (- l *pc* 7))
	(cond
		((<= -0x80000000 l 0x7fffffff)
			(emitm-byte (+ 0x48 (<< dh 2)) o (+ 0x05 (<< dl 3)))
			(emitm-int l))
		(:t (throw "x64-pr constant out of range !" l))))

(defun x64-alu-cr (digit rax_op c r)
	(defq rl (logand 7 r) rh (>> r 3))
	(emitm-byte (+ 0x48 rh))
	(cond
		((<= -0x80 c 0x7f)
			(emitm-byte 0x83 (+ digit rl) c))
		((<= -0x80000000 c 0x7fffffff)
			(if (= r 0)
				(emitm-byte rax_op)
				(emitm-byte 0x81 (+ digit rl)))
			(emitm-int c))
		(:t (throw "x64-alu-cr constant out of range !" c))))

(defun x64-shift-cr (digit c r)
	(unless (= c 0)
		(defq rl (logand 7 r) rh (>> r 3))
		(emitm-byte (+ 0x48 rh))
		(cond
			((= c 1)
				(emitm-byte 0xd1 (+ digit rl)))
			((<= c 0xff)
				(emitm-byte 0xc1 (+ digit rl) c))
			(:t (throw "x64-shift-cr constant out of range !" c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Low-Level Stack & Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-push (&rest b)
	(each (lambda (r)
		(if (>= r 8)
			(emitm-byte 0x41 (+ 0x48 r))
			(emitm-byte (+ 0x50 r)))) b))

(defun emit-pop (&rest b)
	(reach (lambda (r)
		(if (>= r 8)
			(emitm-byte 0x41 (+ 0x50 r))
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
(defun emit-xor-rr (s d) (x64-r-r 0x31 s d))

(defun emit-or-cr (c r)
	(unless (= c 0)
		(x64-alu-cr 0xc8 0x0d c r)))

(defun emit-cpy-cr (c r)
	(if (= c 0)
		(emit-xor-rr r r)
		(progn
			(defq rl (logand r 7) rh (>> r 3))
			(cond
				((= -1 c)
					(emit-or-cr -1 r))
				((<= 0 c 0xffffffff)
					(if (>= r 8) (emitm-byte 0x41))
					(emitm-byte (+ 0xb8 rl))
					(emitm-int c))
				((<= -0x80000000 c -1)
					(emitm-byte (+ 0x48 rh) 0xc7 (+ 0xc0 rl))
					(emitm-int c))
				(:t (emitm-byte (+ 0x48 rh) (+ 0xb8 rl))
					(emitm-long c))))))

(defun emit-add-cr (c r)
	(unless (= c 0)
		(if (= c 1)
			(emitm-byte (+ 0x48 (>> r 3)) 0xff (+ 0xc0 (logand 7 r)))
			(x64-alu-cr 0xc0 0x05 c r))))

(defun emit-sub-cr (c r)
	(unless (= c 0)
		(if (= c 1)
			(emitm-byte (+ 0x48 (>> r 3)) 0xff (+ 0xc8 (logand 7 r)))
			(x64-alu-cr 0xe8 0x2d c r))))

(defun emit-and-cr (c r)
	(unless (= c -1)
		(if (= c 0)
			(emit-xor-rr r r)
			(x64-alu-cr 0xe0 0x25 c r))))

(defun emit-xor-cr (c r)
	(unless (= c 0)
		(x64-alu-cr 0xf0 0x35 c r)))

(defun x64-cmp-rr (s d) (x64-r-r 0x39 s d))
(defun x64-cmp-cr (c r) (if (= c 0) (x64-r-r 0x85 r r) (x64-alu-cr 0xf8 0x3d c r)))

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) (const (emit-native-reg? :rsp))))
(defun emit-free (c) (emit-add-cr (align c stack_align) (const (emit-native-reg? :rsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 2: Memory Load & Store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-ir (s c d) (x64-mem-ir 1 :nil 0x8b d s c))
(defun emit-cpy-ir-b (s c d) (x64-mem-ir 1 :nil '(0x0f 0xbe) d s c))
(defun emit-cpy-ir-s (s c d) (x64-mem-ir 1 :nil '(0x0f 0xbf) d s c))
(defun emit-cpy-ir-i (s c d) (x64-mem-ir 1 :nil 0x63 d s c))
(defun emit-cpy-ir-ub (s c d) (x64-mem-ir 1 :nil '(0x0f 0xb6) d s c))
(defun emit-cpy-ir-us (s c d) (x64-mem-ir 1 :nil '(0x0f 0xb7) d s c))
(defun emit-cpy-ir-ui (s c d) (x64-mem-ir 0 :nil 0x8b d s c))
(defun emit-lea-i (s c d) (unless (and (eql c 0) (eql s d)) (x64-mem-ir 1 :nil 0x8d d s c)))

(defun emit-cpy-ri (s d c) (x64-mem-ir 1 :nil 0x89 s d c))
(defun emit-cpy-ri-b (s d c) (x64-mem-ir :b :nil 0x88 s d c))
(defun emit-cpy-ri-s (s d c) (x64-mem-ir 0 0x66 0x89 s d c))
(defun emit-cpy-ri-i (s d c) (x64-mem-ir 0 :nil 0x89 s d c))

(defun emit-cpy-dr (s1 s2 d) (x64-mem-dr 1 :nil 0x8b d s1 s2))
(defun emit-cpy-dr-b (s1 s2 d) (x64-mem-dr 1 :nil '(0x0f 0xbe) d s1 s2))
(defun emit-cpy-dr-s (s1 s2 d) (x64-mem-dr 1 :nil '(0x0f 0xbf) d s1 s2))
(defun emit-cpy-dr-i (s1 s2 d) (x64-mem-dr 1 :nil 0x63 d s1 s2))
(defun emit-cpy-dr-ub (s1 s2 d) (x64-mem-dr 1 :nil '(0x0f 0xb6) d s1 s2))
(defun emit-cpy-dr-us (s1 s2 d) (x64-mem-dr 1 :nil '(0x0f 0xb7) d s1 s2))
(defun emit-cpy-dr-ui (s1 s2 d) (x64-mem-dr 0 :nil 0x8b d s1 s2))
(defun emit-lea-d (s1 s2 d) (x64-mem-dr 1 :nil 0x8d d s1 s2))

(defun emit-cpy-rd (s d1 d2) (x64-mem-dr 1 :nil 0x89 s d1 d2))
(defun emit-cpy-rd-b (s d1 d2) (x64-mem-dr :b :nil 0x88 s d1 d2))
(defun emit-cpy-rd-s (s d1 d2) (x64-mem-dr 0 0x66 0x89 s d1 d2))
(defun emit-cpy-rd-i (s d1 d2) (x64-mem-dr 0 :nil 0x89 s d1 d2))

(defun emit-cpy-pr (l d) (x64-pr 0x8b l d))
(defun emit-lea-p (l d) (x64-pr 0x8d l d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 3: Swaps, Shifts, Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(if (eql s 0) (setq s d d 0))
		(defq sl (logand 7 s) sh (>> s 3) dl (logand 7 d) dh (>> d 3))
		(if (= d 0)
			(emitm-byte (+ 0x48 sh) (+ 0x90 sl))
			(emitm-byte (+ 0x48 (<< sh 2) dh) 0x87 (+ 0xc0 (<< sl 3) dl)))))

(defun x64-shift-rr (digit s d)
	(cond
		((eql s 1)
			(defq dl (logand 7 d) dh (>> d 3))
			(emitm-byte (+ 0x48 dh) 0xd3 (+ digit dl)))
		((eql d 1)
			(defq sl (logand 7 s) sh (>> s 3))
			(emit-swp-rr s d)
			(emitm-byte (+ 0x48 sh) 0xd3 (+ digit sl))
			(emit-swp-rr s d))
		(:t (emit-push d s (const (emit-native-reg? :r0)) (const (emit-native-reg? :r1)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 24 (const (emit-native-reg? :r0)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 16 (const (emit-native-reg? :r1)))
			(emitm-byte 0x48 0xd3 digit)
			(emit-cpy-ri (const (emit-native-reg? :r0)) (const (emit-native-reg? :rsp)) 24)
			(emit-pop d s (const (emit-native-reg? :r0)) (const (emit-native-reg? :r1))))))

(defun emit-shl-cr (c r) (x64-shift-cr 0xe0 c r))
(defun emit-shr-cr (c r) (x64-shift-cr 0xe8 c r))
(defun emit-asr-cr (c r) (x64-shift-cr 0xf8 c r))

(defun emit-shl-rr (s d) (x64-shift-rr 0xe0 s d))
(defun emit-shr-rr (s d) (x64-shift-rr 0xe8 s d))
(defun emit-asr-rr (s d) (x64-shift-rr 0xf8 s d))

(defun emit-lnot-rr (r d)
	(if (>= d 4)
		(throw "emit-lnot-rr illegal register !" d))
	(emit-cpy-rr d r)
	(emit-xor-rr d d)
	(x64-cmp-cr 0 r)
	(emitm-byte 0x0f 0x94 (+ 0xc0 d)))

(defun emit-land-rr (s d)
	(if (or (>= s 4) (>= d 4))
		(throw "emit-land-rr illegal register !" (list s d)))
	(x64-cmp-cr 0 d)
	(emitm-byte 0x0f 0x95 (+ 0xc0 d))
	(x64-cmp-cr 0 s)
	(emitm-byte 0x0f 0x95 (+ 0xc0 s))
	(emit-and-rr s d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 4: Multiply, Divide, Ext
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-ext-rr (s d)
	(cond
		((and (eql s 0) (eql d 2))
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
					(emitm-byte (+ 0x48 rh) 0xf7 (+ 0xd8 rl)))
				((<= -0x80 c 0x7f)
					(emitm-byte (+ 0x48 (<< rh 2) rh) 0x6b (+ 0xc0 rl (<< rl 3)) c))
				((<= -0x80000000 c 0x7fffffff)
					(emitm-byte (+ 0x48 (<< rh 2) rh) 0x69 (+ 0xc0 rl (<< rl 3)))
					(emitm-int c))
				(:t (throw "emit-mul-cr constant out of range !" c))))))

(defun emit-div-rrr (s d1 d2)
	(cond
		((and (eql d1 2) (eql d2 0))
			(defq sl (logand 7 s) sh (>> s 3))
			(emitm-byte (+ 0x48 sh) 0xf7 (+ 0xf8 sl)))
		((and (eql d1 0) (eql d2 2) (nql s 0) (nql s 2))
			(defq sl (logand 7 s) sh (>> s 3))
			(emitm-byte 0x48 0x92 (+ 0x48 sh) 0xf7 (+ 0xf8 sl) 0x48 0x92))
		(:t (emit-push d2 d1 s (const (emit-native-reg? :r2)) (const (emit-native-reg? :r1)) (const (emit-native-reg? :r0)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 24 (const (emit-native-reg? :r1)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 32 (const (emit-native-reg? :r2)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 40 (const (emit-native-reg? :r0)))
			(emitm-byte 0x48 0xf7 0xf9)
			(emit-cpy-ri (const (emit-native-reg? :r2)) (const (emit-native-reg? :rsp)) 32)
			(emit-cpy-ri (const (emit-native-reg? :r0)) (const (emit-native-reg? :rsp)) 40)
			(emit-pop d2 d1 s (const (emit-native-reg? :r2)) (const (emit-native-reg? :r1)) (const (emit-native-reg? :r0))))))

(defun emit-div-rrr-u (s d1 d2)
	(cond
		((and (eql d1 2) (eql d2 0))
			(defq sl (logand 7 s) sh (>> s 3))
			(emitm-byte (+ 0x48 sh) 0xf7 (+ 0xf0 sl)))
		((and (eql d1 0) (eql d2 2) (nql s 0) (nql s 2))
			(defq sl (logand 7 s) sh (>> s 3))
			(emitm-byte 0x48 0x92 (+ 0x48 sh) 0xf7 (+ 0xf0 sl) 0x48 0x92))
		(:t (emit-push d2 d1 s (const (emit-native-reg? :r2)) (const (emit-native-reg? :r1)) (const (emit-native-reg? :r0)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 24 (const (emit-native-reg? :r1)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 32 (const (emit-native-reg? :r2)))
			(emit-cpy-ir (const (emit-native-reg? :rsp)) 40 (const (emit-native-reg? :r0)))
			(emitm-byte 0x48 0xf7 0xf1)
			(emit-cpy-ri (const (emit-native-reg? :r2)) (const (emit-native-reg? :rsp)) 32)
			(emit-cpy-ri (const (emit-native-reg? :r0)) (const (emit-native-reg? :rsp)) 40)
			(emit-pop d2 d1 s (const (emit-native-reg? :r2)) (const (emit-native-reg? :r1)) (const (emit-native-reg? :r0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 5: Branches & Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x64-branch (o l d)
	(defq m (elem-get *offsets* d) l (- l *pc*))
	(and (> *pass* 0) (> (abs l) (abs m)) (elem-set *offsets* d (setq m l)))
	(cond
		((<= -0x80 (- m 2) 0x7f)
			(emitm-byte o (- l 2)))
		((<= -0x80000000 (- m 6) 0x7fffffff)
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

(defun emit-seq-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x74 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sne-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x75 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-slt-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x7c 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sle-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x7e 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sgt-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x7f 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sge-cr (c d)
	(x64-cmp-cr c d) (emitm-byte 0x7d 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))

(defun emit-seq-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x74 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sne-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x75 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-slt-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x7c 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sle-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x7e 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sgt-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x7f 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))
(defun emit-sge-rr (s d)
	(x64-cmp-rr s d) (emitm-byte 0x7d 5) (emit-xor-rr d d) (emitm-byte 0xeb 4) (emit-or-cr -1 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 7: Min, Max, Abs, CMOV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-min-cr (c d l m)
	(emit-ble-cr c d l m)
	(emit-cpy-cr c d))

(defun emit-max-cr (c d l m)
	(emit-bge-cr c d l m)
	(emit-cpy-cr c d))

(defun emit-min-rr (s d l m)
	(unless (eql s d)
		(x64-cmp-rr s d)
		(x64-0f-r-r 0x4f s d)))

(defun emit-max-rr (s d l m)
	(unless (eql s d)
		(x64-cmp-rr s d)
		(x64-0f-r-r 0x4c s d)))

(defun emit-abs-rr (s d l m)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-bge-cr 0 d l m)
	(emit-mul-cr -1 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 8: Calls and Jumps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-call (l)
	(cond
		((<= -0x80000000 (defq c (- l *pc* 5)) 0x7fffffff)
			(emitm-byte 0xe8)
			(emitm-int c))
		(:t (throw "emit-call constant out of range !" c))))

(defun emit-jmp (l d)
	(defq m (elem-get *offsets* d) c (- l *pc*))
	(and (> *pass* 0) (> (abs c) (abs m)) (elem-set *offsets* d (setq m c)))
	(cond
		((<= -0x80 (- m 2) 0x7f)
			(emitm-byte 0xeb (- c 2)))
		((<= -0x80000000 (- m 5) 0x7fffffff)
			(emitm-byte 0xe9)
			(emitm-int (- c 5)))
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-call-r (r &ignore)
	(defq rl (logand 7 r))
	(if (>= r 8) (emitm-byte 0x41))
	(emitm-byte 0xff (+ 0xd0 rl)))

(defun emit-jmp-r (r &ignore)
	(defq rl (logand 7 r))
	(if (>= r 8) (emitm-byte 0x41))
	(emitm-byte 0xff (+ 0xe0 rl)))

(defun emit-call-i (b c &ignore) (x64-mem-ir 0 :nil 0xff 2 b c))
(defun emit-jmp-i (b c &ignore) (x64-mem-ir 0 :nil 0xff 4 b c))

(defun emit-call-p (l)
	(emitm-byte 0xff 0x15)
	(emitm-int (- l *pc* 4)))

(defun emit-jmp-p (l)
	(emitm-byte 0xff 0x25)
	(emitm-int (- l *pc* 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 9: Floating Point (SSE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x64-shift-xmm (d digit i)
	(defq dh (>> d 3) dl (logand 7 d))
	(emitm-byte 0x66)
	(if (>= d 8) (emitm-byte (+ 0x40 dh)))
	(emitm-byte 0x0f 0x73 (+ 0xc0 (<< digit 3) dl) i))

(defun emit-cpy-ff (s d) (unless (eql s d) (x64-xmm-rr 0xf2 0x10 s d)))
(defun emit-cpy-rf (s d) (x64-gpr-xmm-rr 0x66 0x6e s d))
(defun emit-cpy-fr (s d) (x64-gpr-xmm-rr 0x66 0x7e d s))
(defun emit-cvt-rf (s d) (x64-gpr-xmm-rr 0xf2 0x2a s d))
(defun emit-cvt-fr (s d) (x64-gpr-xmm-rr 0xf2 0x2c s d))

(defun emit-add-ff (s d) (x64-xmm-rr 0xf2 0x58 s d))
(defun emit-sub-ff (s d) (x64-xmm-rr 0xf2 0x5c s d))
(defun emit-mul-ff (s d) (x64-xmm-rr 0xf2 0x59 s d))
(defun emit-div-ff (s d) (x64-xmm-rr 0xf2 0x5e s d))
(defun emit-min-ff (s d) (x64-xmm-rr 0xf2 0x5d s d))
(defun emit-max-ff (s d) (x64-xmm-rr 0xf2 0x5f s d))
(defun emit-sqrt-ff (s d) (x64-xmm-rr 0xf2 0x51 s d))

(defun emit-abs-ff (s d)
	(unless (eql s d) (emit-cpy-ff s d))
	(x64-shift-xmm d 6 1)
	(x64-shift-xmm d 2 1))

(defun emit-neg-ff (s d)
	(emit-push 0)
	(emit-cpy-fr s 0)
	(emitm-byte 0x48 0x0f 0xba 0xf8 63)
	(emit-cpy-rf 0 d)
	(emit-pop 0))

(defun x64-cmp-ff (s d) (x64-xmm-rr 0x66 0x2e s d))
(defun emit-beq-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x74 l m))
(defun emit-bne-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x75 l m))
(defun emit-blt-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x72 l m))
(defun emit-bgt-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x77 l m))
(defun emit-ble-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x76 l m))
(defun emit-bge-ff (s d l m) (x64-cmp-ff s d) (x64-branch 0x73 l m))

(defun emit-cpy-if (s c d) (x64-mem-ir 1 0xf2 '(0x0f 0x10) d s c))
(defun emit-cpy-fi (s d c) (x64-mem-ir 1 0xf2 '(0x0f 0x11) s d c))

(defun x64-sse-mem-dr (o reg s1 s2)
	(if (or (= s2 4) (= s2 12))
		(defq tmp s1 s1 s2 s2 tmp))
	(x64-mem-dr 0 0xf2 o reg s1 s2))

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
			(emit-add-cr (* +ptr_size (length x)) (const (emit-native-reg? :rsp))))
		(:t (setq x (cat (list r r) x))
			(emit-cpy-rr (const (emit-native-reg? :rsp)) r)
			(emit-and-cr -16 (const (emit-native-reg? :rsp)))
			(apply (const emit-push) x)
			(emit-call-i b c)
			(emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (- (length x) 2)) (const (emit-native-reg? :rsp)))))))
(WIN64
(defun emit-call-abi (r b c n &rest x)
	(cond
		((= *func_align* 16)
			(if (odd? (length x))
				(setq x (cat (list r) x)))
			(apply (const emit-push) x)
			(emit-sub-cr 32 (const (emit-native-reg? :rsp)))
			(emit-call-i b c)
			(emit-add-cr (* +ptr_size (+ (length x) 4)) (const (emit-native-reg? :rsp))))
		(:t (emit-cpy-rr (const (emit-native-reg? :rsp)) r)
			(emit-and-cr -16 (const (emit-native-reg? :rsp)))
			(if (odd? (length x))
				(defq y (cat (list r) x))
				(defq y (cat (list r r) x)))
			(apply (const emit-push) y)
			(emit-sub-cr 32 (const (emit-native-reg? :rsp)))
			(emit-call-i b c)
			(emit-cpy-ir (const (emit-native-reg? :rsp)) (* +ptr_size (+ (length x) 4)) (const (emit-native-reg? :rsp)))))))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

;module, that exports everything !
(export-symbols (map (const first) (tolist (env))))
;plus any private instruction maps if validating
(validatecase
	(defq emit_maps (list))
	(export-symbols '(emit_maps)))
(env-pop)