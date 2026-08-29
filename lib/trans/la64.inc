;;;;;;;;;;;;;;;;;;;;;;;;
; LA64 Emit Functions
;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

;cpu specific requirements
;$r1 (ra) is link register
(defcvar 'stack_align 8 'stack_state '(:r30))

(defq +emit_native_regs
	(reduce (# (pinsert %0 %1 (num (!))))
		'(:r29 :r30 :r28 :rsp							;$r0 (zero), $r1 (ra), $r2 (tp), $r3 (sp)
		:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7					;$r4-$r11 ($a0-$a7)
		:r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25	;$r12-$r20 ($t0-$t8)
		:r26 :r27										;$r21-$r22 ($fp, $s9)
		:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15 :r16)		;$r23-$r31 ($s0-$s8)
	(reduce (# (pinsert %0 %1 (num (!))))
		'(:f0 :f1 :f2 :f3 :f4 :f5 :f6 :f7				;$f0-$f7 ($fa0-$fa7)
		:f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15			;$f8-$f15 ($ft0-$ft7)
		:f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23			;$f16-$f23 ($ft8-$ft15)
		:f24 :f25 :f26 :f27 :f28 :f29 :f30 :f31)		;$f24-$f31 ($fs0-$fs7)
		(cat +vp_regs_map))))

(defmacro emit-native-reg? (r) (static-qq (pfind +emit_native_regs ,r)))

(defq +la64_rzero (emit-native-reg? :r29)			;$r0 (zero)
	+la64_rlnk  (emit-native-reg? :r30)				;$r1 (ra)
	+la64_rsp   (emit-native-reg? :rsp)				;$r3 (sp)
	+la64_rtmp1 (emit-native-reg? :r17)				;$r12 (t0)
	+la64_rtmp2 (emit-native-reg? :r18)				;$r13 (t1)
	+la64_rtmp3 (emit-native-reg? :r19)				;$r14 (t2)
	+la64_rtmp4 (emit-native-reg? :r15)				;$r30 (s7 - callee-saved)
	+la64_rtmp5 (emit-native-reg? :r16))			;$r31 (s8 - callee-saved)

;;;;;;;;;;;;;;;;;;;;;;;;
; Core Encoding Macros
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro la64-within (c n)
	(defq limit (<< 1 (dec n)))
	`(<= ,(neg limit) ,c ,(dec limit)))

(defmacro la64-bf (n width lsb pos)
	`(<< (logand (>> ,n ,lsb) ,(dec (<< 1 width))) ,pos))

; 3R Format: [31:15] op | [14:10] rk | [9:5] rj | [4:0] rd
(defmacro la64-3r (o rd rj rk)
	`(emitm-int (+ ,o (<< ,rk 10) (<< ,rj 5) ,rd)))

; 2RI12 Format: [31:22] op | [21:10] imm12 | [9:5] rj | [4:0] rd
(defmacro la64-2ri12 (o rd rj imm12)
	`(emitm-int (+ ,o (<< (logand ,imm12 0xfff) 10) (<< ,rj 5) ,rd)))

; 2RI14 Format: [31:24] op | [23:10] imm14 | [9:5] rj | [4:0] rd
(defmacro la64-2ri14 (o rd rj imm14)
	`(emitm-int (+ ,o (<< (logand ,imm14 0x3fff) 10) (<< ,rj 5) ,rd)))

; 2RI16 (Branch 16-bit) Format: [31:26] op | [25:10] offs[17:2] | [9:5] rj | [4:0] rd
(defmacro la64-b16 (o rj rd offs18)
	`(emitm-int (+ ,o (<< ,rj 5) ,rd
		(la64-bf ,offs18 16 2 10))))

; 1RI20 Format: [31:25] op | [24:5] imm20 | [4:0] rd
(defmacro la64-1ri20 (o rd imm20)
	`(emitm-int (+ ,o (<< (logand ,imm20 0xfffff) 5) ,rd)))

; B26 (Jump / Call 26-bit) Format: [31:26] op | [25:10] offs[17:2] | [9:0] offs[27:18]
(defmacro la64-b26 (o offs28)
	`(emitm-int (+ ,o
		(la64-bf ,offs28 16 2 10)
		(la64-bf ,offs28 10 18 0))))

; JIRL Format: [31:26] op | [25:10] offs[17:2] | [9:5] rj | [4:0] rd
(defmacro la64-jirl (rd rj offs18)
	`(emitm-int (+ 0x4c000000 (<< ,rj 5) ,rd
		(la64-bf ,offs18 16 2 10))))

;;;;;;;;;;;;;;;;;;;;;;
; Group 1: Shifts (RR & CR)
;;;;;;;;;;;;;;;;;;;;;;

(defun emit-shl-rr (s d) (la64-3r 0x00188000 d d s)) ; sll.d
(defun emit-shr-rr (s d) (la64-3r 0x00190000 d d s)) ; srl.d
(defun emit-asr-rr (s d) (la64-3r 0x00198000 d d s)) ; sra.d

(defun emit-shl-cr (c r) (la64-3r 0x00410000 r r (logand c 0x3f))) ; slli.d
(defun emit-shr-cr (c r) (la64-3r 0x00450000 r r (logand c 0x3f))) ; srli.d
(defun emit-asr-cr (c r) (la64-3r 0x00490000 r r (logand c 0x3f))) ; srai.d

;;;;;;;;;;;;;;;;;;;;;;
; Group 2: ALU (RR)
;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-rr (s d) (unless (eql s d) (la64-2ri12 0x02c00000 d s 0))) ; addi.d d, s, 0
(defun emit-add-rr (s d) (la64-3r 0x00108000 d d s))                     ; add.d d, d, s
(defun emit-sub-rr (s d) (la64-3r 0x00118000 d d s))                     ; sub.d d, d, s
(defun emit-mul-rr (s d) (la64-3r 0x001d8000 d d s))                     ; mul.d d, d, s
(defun emit-and-rr (s d) (unless (eql s d) (la64-3r 0x00148000 d d s)))  ; and d, d, s
(defun emit-or-rr  (s d) (unless (eql s d) (la64-3r 0x00150000 d d s)))  ; or d, d, s
(defun emit-xor-rr (s d) (la64-3r 0x00158000 d d s))                     ; xor d, d, s

(defun emit-div-rrr (s d1 d2)
	(la64-3r 0x00228000 +la64_rtmp1 d2 s) ; mod.d tmp, d2, s (remainder)
	(la64-3r 0x00220000 d1 d2 s)          ; div.d d1, d2, s  (quotient)
	(emit-cpy-rr +la64_rtmp1 d2))

(defun emit-div-rrr-u (s d1 d2)
	(la64-3r 0x00238000 +la64_rtmp1 d2 s) ; mod.du tmp, d2, s (remainder)
	(la64-3r 0x00230000 d1 d2 s)          ; div.du d1, d2, s  (quotient)
	(emit-cpy-rr +la64_rtmp1 d2))

;;;;;;;;;;;;;;;;;;;;;;
; Group 3: ALU (CR)
;;;;;;;;;;;;;;;;;;;;;;

(defun la64-cr (c r)
	(cond
		((la64-within c 12)
			(la64-2ri12 0x02c00000 r +la64_rzero c))
		((la64-within c 32)
			(defq l (logand c 0xfff) u (>>> (logand c 0xfffff000) 12))
			(la64-1ri20 0x14000000 r u) ; lu12i.w
			(if (/= l 0) (la64-2ri12 0x03800000 r r l))) ; ori
		(:t (defq l12 (logand c 0xfff)
				u20 (>>> (logand c 0xfffff000) 12)
				m20 (>>> (logand c 0xfffff00000000) 32)
				h12 (>>> c 52))
			(la64-1ri20 0x14000000 r u20) ; lu12i.w
			(if (/= l12 0) (la64-2ri12 0x03800000 r r l12)) ; ori
			(if (/= m20 0) (la64-1ri20 0x16000000 r m20)) ; lu32i.d
			(if (/= h12 0) (la64-2ri12 0x03000000 r r h12))))) ; lu52i.d

(defun emit-cpy-cr (c r) (la64-cr c r))

(defun emit-add-cr (c r)
	(cond
		((= c 0))
		((la64-within c 12)
			(la64-2ri12 0x02c00000 r r c))
		(:t (emit-cpy-cr c +la64_rtmp1)
			(emit-add-rr +la64_rtmp1 r))))

(defun emit-sub-cr (c r) (emit-add-cr (neg c) r))

(defun emit-and-cr (c r)
	(cond
		((= c -1))
		((= c 0)
			(emit-xor-rr r r))
		((<= 0 c 0xfff)
			(la64-2ri12 0x03400000 r r c)) ; andi
		(:t (emit-cpy-cr c +la64_rtmp1)
			(emit-and-rr +la64_rtmp1 r))))

(defun emit-or-cr (c r)
	(cond
		((= c 0))
		((<= 0 c 0xfff)
			(la64-2ri12 0x03800000 r r c)) ; ori
		(:t (emit-cpy-cr c +la64_rtmp1)
			(emit-or-rr +la64_rtmp1 r))))

(defun emit-xor-cr (c r)
	(cond
		((= c 0))
		((<= 0 c 0xfff)
			(la64-2ri12 0x03c00000 r r c)) ; xori
		(:t (emit-cpy-cr c +la64_rtmp1)
			(emit-xor-rr +la64_rtmp1 r))))

(defun emit-mul-cr (c r)
	(cond
		((= c 1))
		((= c 0)
			(emit-xor-rr r r))
		((= c -1)
			(la64-3r 0x00118000 r +la64_rzero r)) ; sub.d r, zero, r
		((defq s (log2 c))
			(emit-shl-cr s r))
		(:t (emit-cpy-cr c +la64_rtmp1)
			(emit-mul-rr +la64_rtmp1 r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 4: Swap & Sign Extend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-swp-rr (s d)
	(unless (eql s d)
		(emit-cpy-rr s +la64_rtmp1)
		(emit-cpy-rr d s)
		(emit-cpy-rr +la64_rtmp1 d)))

(defun emit-ext-rr (s d)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-asr-cr 63 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 5: Branches & Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la64-branch (o d s l)
	(defq c (- l *pc*))
	(cond
		((la64-within c 18)
			(la64-b16 o d s c))
		(:t (la64-b16 (logxor o 0x04000000) d s 8) ; invert condition, skip 8 bytes
			(la64-b26 0x50000000 (- l *pc*)))))    ; b target

(defun emit-beq-rr (s d l m) (la64-branch 0x58000000 d s l)) ; beq
(defun emit-bne-rr (s d l m) (la64-branch 0x5c000000 d s l)) ; bne
(defun emit-blt-rr (s d l m) (la64-branch 0x60000000 d s l)) ; blt
(defun emit-bge-rr (s d l m) (la64-branch 0x64000000 d s l)) ; bge
(defun emit-bgt-rr (s d l m) (la64-branch 0x60000000 s d l)) ; blt s, d
(defun emit-ble-rr (s d l m) (la64-branch 0x64000000 s d l)) ; bge s, d

(defun emit-beq-cr (c d l m)
	(cond
		((= c 0) (emit-beq-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-beq-rr +la64_rtmp1 d l m))))
(defun emit-bne-cr (c d l m)
	(cond
		((= c 0) (emit-bne-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-bne-rr +la64_rtmp1 d l m))))
(defun emit-blt-cr (c d l m)
	(cond
		((= c 0) (emit-blt-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-blt-rr +la64_rtmp1 d l m))))
(defun emit-bgt-cr (c d l m)
	(cond
		((= c 0) (emit-bgt-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-bgt-rr +la64_rtmp1 d l m))))
(defun emit-ble-cr (c d l m)
	(cond
		((= c 0) (emit-ble-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-ble-rr +la64_rtmp1 d l m))))
(defun emit-bge-cr (c d l m)
	(cond
		((= c 0) (emit-bge-rr +la64_rzero d l m))
		(:t (emit-cpy-cr c +la64_rtmp1) (emit-bge-rr +la64_rtmp1 d l m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 6: Logic (LNOT & LAND)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-lnot-rr (r d)
	(la64-2ri12 0x02400000 d r 1)) ; sltui d, r, 1

(defun emit-land-rr (s d)
	(la64-3r 0x00128000 +la64_rtmp1 +la64_rzero s) ; sltu tmp, zero, s
	(la64-3r 0x00128000 d +la64_rzero d)          ; sltu d, zero, d
	(la64-3r 0x00148000 d d +la64_rtmp1))         ; and d, d, tmp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 7: Comparisons & Sxx Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-seq-rr (s d)
	(emit-sub-rr s d)
	(la64-2ri12 0x02400000 d d 1)) ; sltui d, d, 1
(defun emit-sne-rr (s d)
	(emit-sub-rr s d)
	(la64-3r 0x00128000 d +la64_rzero d)) ; sltu d, zero, d
(defun emit-slt-rr (s d) (la64-3r 0x00120000 d d s)) ; slt d, d, s
(defun emit-sgt-rr (s d) (la64-3r 0x00120000 d s d)) ; slt d, s, d
(defun emit-sle-rr (s d)
	(la64-3r 0x00120000 +la64_rtmp1 s d)
	(la64-2ri12 0x03c00000 d +la64_rtmp1 1)) ; xori d, tmp, 1
(defun emit-sge-rr (s d)
	(la64-3r 0x00120000 +la64_rtmp1 d s)
	(la64-2ri12 0x03c00000 d +la64_rtmp1 1)) ; xori d, tmp, 1

(defun emit-seq-cr (c d)
	(cond
		((= c 0) (la64-2ri12 0x02400000 d d 1))
		(:t (emit-cpy-cr c +la64_rtmp3) (emit-seq-rr +la64_rtmp3 d))))
(defun emit-sne-cr (c d)
	(cond
		((= c 0) (la64-3r 0x00128000 d +la64_rzero d))
		(:t (emit-cpy-cr c +la64_rtmp3) (emit-sne-rr +la64_rtmp3 d))))
(defun emit-slt-cr (c d)
	(cond
		((la64-within c 12) (la64-2ri12 0x02000000 d d c))
		(:t (emit-cpy-cr c +la64_rtmp3) (emit-slt-rr +la64_rtmp3 d))))
(defun emit-sgt-cr (c d)
	(emit-cpy-cr c +la64_rtmp3)
	(emit-sgt-rr +la64_rtmp3 d))
(defun emit-sle-cr (c d)
	(emit-cpy-cr c +la64_rtmp3)
	(emit-sle-rr +la64_rtmp3 d))
(defun emit-sge-cr (c d)
	(emit-cpy-cr c +la64_rtmp3)
	(emit-sge-rr +la64_rtmp3 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 8: Memory Load & Store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun la64-ir (o s i d)
	(cond
		((la64-within i 12)
			(la64-2ri12 o d s i))
		(:t (emit-cpy-cr i +la64_rtmp3)
			(emit-add-rr s +la64_rtmp3)
			(la64-2ri12 o d +la64_rtmp3 0))))

(defun la64-ri (o s d i)
	(cond
		((la64-within i 12)
			(la64-2ri12 o s d i))
		(:t (emit-cpy-cr i +la64_rtmp3)
			(emit-add-rr d +la64_rtmp3)
			(la64-2ri12 o s +la64_rtmp3 0))))

(defun emit-cpy-ir-b  (s i d) (la64-ir 0x28000000 s i d)) ; ld.b
(defun emit-cpy-ir-s  (s i d) (la64-ir 0x28400000 s i d)) ; ld.h
(defun emit-cpy-ir-i  (s i d) (la64-ir 0x28800000 s i d)) ; ld.w
(defun emit-cpy-ir    (s i d) (la64-ir 0x28c00000 s i d)) ; ld.d
(defun emit-cpy-ir-ub (s i d) (la64-ir 0x2a000000 s i d)) ; ld.bu
(defun emit-cpy-ir-us (s i d) (la64-ir 0x2a400000 s i d)) ; ld.hu
(defun emit-cpy-ir-ui (s i d) (la64-ir 0x2a800000 s i d)) ; ld.wu

(defun emit-cpy-ri-b  (s d i) (la64-ri 0x29000000 s d i)) ; st.b
(defun emit-cpy-ri-s  (s d i) (la64-ri 0x29400000 s d i)) ; st.h
(defun emit-cpy-ri-i  (s d i) (la64-ri 0x29800000 s d i)) ; st.w
(defun emit-cpy-ri    (s d i) (la64-ri 0x29c00000 s d i)) ; st.d

(defun emit-cpy-dr-b  (s i d) (la64-3r 0x38000000 d s i)) ; ldx.b
(defun emit-cpy-dr-s  (s i d) (la64-3r 0x38040000 d s i)) ; ldx.h
(defun emit-cpy-dr-i  (s i d) (la64-3r 0x38080000 d s i)) ; ldx.w
(defun emit-cpy-dr    (s i d) (la64-3r 0x380c0000 d s i)) ; ldx.d
(defun emit-cpy-dr-ub (s i d) (la64-3r 0x38200000 d s i)) ; ldx.bu
(defun emit-cpy-dr-us (s i d) (la64-3r 0x38240000 d s i)) ; ldx.hu
(defun emit-cpy-dr-ui (s i d) (la64-3r 0x38280000 d s i)) ; ldx.wu

(defun emit-cpy-rd-b  (s d i) (la64-3r 0x38100000 s d i)) ; stx.b
(defun emit-cpy-rd-s  (s d i) (la64-3r 0x38140000 s d i)) ; stx.h
(defun emit-cpy-rd-i  (s d i) (la64-3r 0x38180000 s d i)) ; stx.w
(defun emit-cpy-rd    (s d i) (la64-3r 0x381c0000 s d i)) ; stx.d

(defun emit-lea-i (s i d)
	(cond
		((and (= i 0) (eql s d)))
		((la64-within i 12)
			(la64-2ri12 0x02c00000 d s i))
		(:t (emit-cpy-cr i +la64_rtmp3)
			(la64-3r 0x00108000 d s +la64_rtmp3))))

(defun emit-lea-d (s i d) (la64-3r 0x00108000 d s i)) ; add.d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 9: Stack Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-push (&rest b)
	(when (/= 0 (defq l (length b)))
		(emit-sub-cr (align (* +ptr_size l) stack_align) +la64_rsp)
		(reach (lambda (r) (emit-cpy-ri r +la64_rsp (* (- l 1 (!)) +ptr_size))) b)))

(defun emit-pop (&rest b)
	(when (/= 0 (defq l (length b)))
		(reach (lambda (r) (emit-cpy-ir +la64_rsp (* (- l 1 (!)) +ptr_size) r)) b)
		(emit-add-cr (align (* +ptr_size l) stack_align) +la64_rsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 10: Jumps & Calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-call (l)
	(emit-push +la64_rlnk)
	(defq c (- l *pc*))
	(cond
		((la64-within c 28)
			(la64-b26 0x54000000 c)) ; bl
		(:t (throw "emit-call constant out of range !" c)))
	(emit-pop +la64_rlnk))

(defun emit-jmp (l m)
	(defq c (- l *pc*))
	(cond
		((la64-within c 28)
			(la64-b26 0x50000000 c)) ; b
		(:t (throw "emit-jmp constant out of range !" c))))

(defun emit-call-r (r &ignore)
	(emit-push +la64_rlnk)
	(la64-jirl +la64_rlnk r 0) ; jirl ra, r, 0
	(emit-pop +la64_rlnk))

(defun emit-jmp-r (r &ignore)
	(la64-jirl +la64_rzero r 0)) ; jirl zero, r, 0

(defun emit-call-i (s i &ignore)
	(emit-cpy-ir s i +la64_rtmp1)
	(emit-call-r +la64_rtmp1))

(defun emit-jmp-i (s i &ignore)
	(emit-cpy-ir s i +la64_rtmp1)
	(emit-jmp-r +la64_rtmp1))

(defun emit-lea-p (l d)
	(defq c (- l *pc*))
	(cond
		((la64-within c 32)
			(defq l12 (logand c 0xfff) u20 (>>> (logand c 0xfffff000) 12))
			(la64-1ri20 0x1c000000 d u20) ; pcaddu12i
			(if (/= l12 0) (la64-2ri12 0x02c00000 d d l12))) ; addi.d
		(:t (throw "emit-lea-p constant out of range !" c))))

(defun emit-cpy-pr (l d)
	(setq l (- l *pc*))
	(emit-lea-p *pc* d)
	(emit-cpy-ir d l d))

(defun emit-call-p (l)
	(emit-cpy-pr l +la64_rtmp1)
	(emit-call-r +la64_rtmp1))

(defun emit-jmp-p (l)
	(emit-cpy-pr l +la64_rtmp1)
	(emit-jmp-r +la64_rtmp1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 11: Min, Max, Abs, CMOV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-min-cr (c d l m)
	(emit-cpy-cr c +la64_rtmp1)
	(emit-ble-rr +la64_rtmp1 d l m)
	(emit-cpy-rr +la64_rtmp1 d))

(defun emit-max-cr (c d l m)
	(emit-cpy-cr c +la64_rtmp1)
	(emit-bge-rr +la64_rtmp1 d l m)
	(emit-cpy-rr +la64_rtmp1 d))

(defun emit-min-rr (s d l m)
	(unless (eql s d)
		(emit-ble-rr s d l m)
		(emit-cpy-rr s d)))

(defun emit-max-rr (s d l m)
	(unless (eql s d)
		(emit-bge-rr s d l m)
		(emit-cpy-rr s d)))

(defun emit-abs-rr (s d l m)
	(unless (eql s d) (emit-cpy-rr s d))
	(emit-bge-rr +la64_rzero d l m)
	(emit-mul-cr -1 d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 12: Floating Point (LA64)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-ff (s d) (unless (eql s d) (la64-3r 0x01149800 d s 0))) ; fmov.d
(defun emit-cpy-rf (s d) (la64-3r 0x0114a800 d s 0))                     ; movgr2fr.d
(defun emit-cpy-fr (s d) (la64-3r 0x0114b800 d s 0))                     ; movfr2gr.d

(defun emit-cvt-rf (s d)
	(emit-cpy-rf s d)
	(la64-3r 0x011d3800 d d 0)) ; ffint.d.l

(defun emit-cvt-fr (s d)
	(la64-3r 0x011aa800 +la64_rtmp1 s 0) ; ftintrz.l.d
	(emit-cpy-fr +la64_rtmp1 d))

(defun emit-add-ff (s d) (la64-3r 0x01010000 d d s)) ; fadd.d
(defun emit-sub-ff (s d) (la64-3r 0x01030000 d d s)) ; fsub.d
(defun emit-mul-ff (s d) (la64-3r 0x01050000 d d s)) ; fmul.d
(defun emit-div-ff (s d) (la64-3r 0x01070000 d d s)) ; fdiv.d
(defun emit-max-ff (s d) (la64-3r 0x01090000 d d s)) ; fmax.d
(defun emit-min-ff (s d) (la64-3r 0x010b0000 d d s)) ; fmin.d
(defun emit-sqrt-ff (s d) (la64-3r 0x01144800 d s 0)) ; fsqrt.d
(defun emit-abs-ff (s d) (la64-3r 0x01140800 d s 0))  ; fabs.d
(defun emit-neg-ff (s d) (la64-3r 0x01141800 d s 0))  ; fneg.d

(defmacro la64-fp-cmp (cond s d)
	; Compare s and d to $fcc0 (fcmp.cond.d $fcc0, d, s)
	`(emitm-int (+ 0x0c200000 (<< ,cond 15) (<< ,s 10) (<< ,d 5))))

(defun emit-beq-ff (s d l m)
	(la64-fp-cmp 0x04 s d) ; fcmp.ceq.d $fcc0, d, s
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000100 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f)))) ; bcnez $fcc0

(defun emit-bne-ff (s d l m)
	(la64-fp-cmp 0x04 s d) ; fcmp.ceq.d $fcc0, d, s
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000000 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f)))) ; bceqz $fcc0

(defun emit-blt-ff (s d l m)
	(la64-fp-cmp 0x02 s d) ; fcmp.clt.d $fcc0, d, s (d < s)
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000100 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f))))

(defun emit-bgt-ff (s d l m)
	(la64-fp-cmp 0x02 d s) ; fcmp.clt.d $fcc0, s, d (s < d -> d > s)
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000100 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f))))

(defun emit-ble-ff (s d l m)
	(la64-fp-cmp 0x06 s d) ; fcmp.cle.d $fcc0, d, s (d <= s)
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000100 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f))))

(defun emit-bge-ff (s d l m)
	(la64-fp-cmp 0x06 d s) ; fcmp.cle.d $fcc0, s, d (s <= d -> d >= s)
	(defq c (- l *pc*))
	(emitm-int (+ 0x48000100 (<< (logand (>> c 2) 0xffff) 10) (logand (>> c 18) 0x1f))))

(defun emit-cpy-if (s c d)
	(cond
		((la64-within c 12)
			(la64-2ri12 0x2b800000 d s c)) ; fld.d
		(:t (emit-cpy-cr c +la64_rtmp1)
			(la64-3r 0x38340000 d s +la64_rtmp1)))) ; fldx.d

(defun emit-cpy-fi (s d c)
	(cond
		((la64-within c 12)
			(la64-2ri12 0x2bc00000 s d c)) ; fst.d
		(:t (emit-cpy-cr c +la64_rtmp1)
			(la64-3r 0x383c0000 s d +la64_rtmp1)))) ; fstx.d

(defun emit-cpy-df (s1 s2 d) (la64-3r 0x38340000 d s1 s2)) ; fldx.d
(defun emit-cpy-fd (s d1 d2) (la64-3r 0x383c0000 s d1 d2)) ; fstx.d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Group 13: Stack & ABI Linkage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-alloc (c) (emit-sub-cr (align c stack_align) +la64_rsp))
(defun emit-free (c)  (emit-add-cr (align c stack_align) +la64_rsp))
(defun emit-ret ()    (la64-jirl +la64_rzero +la64_rlnk 0)) ; jirl zero, ra, 0
(defun emit-sync (n)  (emitm-int 0x38720000))               ; dbar 0
(defun emit-brk (n)   (emitm-int 0x002a0000))               ; break 0

(defun emit-stack-init (s f x)
	(defq tk_state_size (* +ptr_size (length stack_state)))
	(emit-sub-cr (+ tk_state_size (* +ptr_size 2)) s)
	(emit-and-cr -16 s)
	(emit-cpy-ri f s tk_state_size)
	(emit-cpy-ri x s (+ tk_state_size +ptr_size))
	(emit-lea-p (+ *pc* 16) +la64_rtmp1)
	(emit-cpy-ri +la64_rtmp1 s 0)
	(emit-jmp (+ *pc* 20) 0)
;la64_start
	(emit-pop +la64_rlnk +la64_rtmp1)
	(emit-jmp-r +la64_rtmp1))
;la64_exit

(case *abi*
(LA64
(defun emit-call-abi (r b c n &rest x)
	(defq s (* +ptr_size (length x)))
	(emit-cpy-rr +la64_rsp +la64_rtmp4)
	(emit-cpy-rr +la64_rlnk +la64_rtmp5)
	(cond
		((= (% *func_align* 16) 0)
			(emit-sub-cr (align s 16) +la64_rsp))
		(:t (emit-sub-cr s +la64_rsp)
			(emit-and-cr -16 +la64_rsp)))
	(each (# (emit-cpy-ri %0 +la64_rsp (* +ptr_size (!)))) (reverse x))
	(emit-cpy-ir b c +la64_rtmp1)
	(la64-jirl +la64_rlnk +la64_rtmp1 0)
	(emit-cpy-rr +la64_rtmp4 +la64_rsp)
	(emit-cpy-rr +la64_rtmp5 +la64_rlnk)))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

;module, that exports everything !
(export-symbols (map (const first) (tolist (env))))
;plus any private instruction maps if validating
(validatecase
	(defq emit_maps (list))
	(export-symbols '(emit_maps)))
(env-pop)