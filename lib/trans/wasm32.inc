;;;;;;;;;;;;;;;;;;;;;
; WASM32 Emit Functions
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

; WebAssembly uses a stack-based VM, but we can use local variables
; to simulate the VP's 16 registers (r0-r14, rsp)
; This is the register mapping strategy

(defcvar 'stack_align 16 'stack_state '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))

(defun emit-native-reg? (r) (find r +vp_regs))

;;;;;;;;;;;;;;;;;;;;;;;;;
; WASM Bytecode Constants
;;;;;;;;;;;;;;;;;;;;;;;;;

; WASM opcodes
(defq +wasm_end 0x0b
	  +wasm_local_get 0x20
	  +wasm_local_set 0x21
	  +wasm_local_tee 0x22
	  +wasm_i64_load 0x29
	  +wasm_i64_load8_s 0x30
	  +wasm_i64_load8_u 0x31
	  +wasm_i64_load16_s 0x32
	  +wasm_i64_load16_u 0x33
	  +wasm_i64_load32_s 0x34
	  +wasm_i64_load32_u 0x35
	  +wasm_i64_store 0x37
	  +wasm_i64_store8 0x3c
	  +wasm_i64_store16 0x3d
	  +wasm_i64_store32 0x3e
	  +wasm_i64_const 0x42
	  +wasm_i64_eqz 0x50
	  +wasm_i64_eq 0x51
	  +wasm_i64_ne 0x52
	  +wasm_i64_lt_s 0x53
	  +wasm_i64_lt_u 0x54
	  +wasm_i64_gt_s 0x55
	  +wasm_i64_gt_u 0x56
	  +wasm_i64_le_s 0x57
	  +wasm_i64_le_u 0x58
	  +wasm_i64_ge_s 0x59
	  +wasm_i64_ge_u 0x5a
	  +wasm_i64_add 0x7c
	  +wasm_i64_sub 0x7d
	  +wasm_i64_mul 0x7e
	  +wasm_i64_div_s 0x7f
	  +wasm_i64_div_u 0x80
	  +wasm_i64_rem_s 0x81
	  +wasm_i64_rem_u 0x82
	  +wasm_i64_and 0x83
	  +wasm_i64_or 0x84
	  +wasm_i64_xor 0x85
	  +wasm_i64_shl 0x86
	  +wasm_i64_shr_s 0x87
	  +wasm_i64_shr_u 0x88
	  +wasm_block 0x02
	  +wasm_loop 0x03
	  +wasm_if 0x04
	  +wasm_else 0x05
	  +wasm_br 0x0c
	  +wasm_br_if 0x0d
	  +wasm_call 0x10
	  +wasm_call_indirect 0x11
	  +wasm_return 0x0f
	  +wasm_drop 0x1a
	  +wasm_select 0x1b)

;;;;;;;;;;;;;;;;;;;;;;;;;
; WASM LEB128 Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-leb128-unsigned (n)
	; Emit unsigned LEB128 encoding
	(while (>= n 0x80)
		(emit-byte (logior (logand n 0x7f) 0x80))
		(setq n (>> n 7)))
	(emit-byte (logand n 0x7f)))

(defun emit-leb128-signed (n)
	; Emit signed LEB128 encoding
	(defq more :t negative (< n 0))
	(while more
		(defq byte (logand n 0x7f))
		(setq n (>> n 7))
		(cond
			((and (= n 0) (not (bits? byte 0x40)))
				(setq more :nil))
			((and (= n -1) (bits? byte 0x40))
				(setq more :nil))
			(:t (setq byte (logior byte 0x80))))
		(emit-byte byte)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Register Operations
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-get-local (r)
	; Load a register value onto the stack
	(emit-byte +wasm_local_get)
	(emit-leb128-unsigned r))

(defun emit-set-local (r)
	; Store stack top into a register
	(emit-byte +wasm_local_set)
	(emit-leb128-unsigned r))

(defun emit-tee-local (r)
	; Store stack top into a register, keeping value on stack
	(emit-byte +wasm_local_tee)
	(emit-leb128-unsigned r))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Constant to Register
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-cr (c r)
	; Copy constant to register: r = c
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-set-local r))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Register to Register
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-rr (s d)
	; Copy register: d = s
	(emit-get-local s)
	(emit-set-local d))

(defun emit-add-rr (s d)
	; Add: d = d + s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_add)
	(emit-set-local d))

(defun emit-sub-rr (s d)
	; Subtract: d = d - s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_sub)
	(emit-set-local d))

(defun emit-mul-rr (s d)
	; Multiply: d = d * s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_mul)
	(emit-set-local d))

(defun emit-and-rr (s d)
	; Bitwise AND: d = d & s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_and)
	(emit-set-local d))

(defun emit-or-rr (s d)
	; Bitwise OR: d = d | s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_or)
	(emit-set-local d))

(defun emit-xor-rr (s d)
	; Bitwise XOR: d = d ^ s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_xor)
	(emit-set-local d))

(defun emit-shl-rr (s d)
	; Shift left: d = d << s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_shl)
	(emit-set-local d))

(defun emit-shr-rr (s d)
	; Shift right unsigned: d = d >> s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_shr_u)
	(emit-set-local d))

(defun emit-asr-rr (s d)
	; Arithmetic shift right: d = d >> s (signed)
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_shr_s)
	(emit-set-local d))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Constant Operations
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-add-cr (c r)
	; Add constant: r = r + c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_add)
		(emit-set-local r)))

(defun emit-sub-cr (c r)
	; Subtract constant: r = r - c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_sub)
		(emit-set-local r)))

(defun emit-mul-cr (c r)
	; Multiply constant: r = r * c
	(unless (= c 1)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_mul)
		(emit-set-local r)))

(defun emit-and-cr (c r)
	; AND constant: r = r & c
	(unless (= c -1)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_and)
		(emit-set-local r)))

(defun emit-or-cr (c r)
	; OR constant: r = r | c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_or)
		(emit-set-local r)))

(defun emit-xor-cr (c r)
	; XOR constant: r = r ^ c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_xor)
		(emit-set-local r)))

(defun emit-shl-cr (c r)
	; Shift left constant: r = r << c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_shl)
		(emit-set-local r)))

(defun emit-shr-cr (c r)
	; Shift right constant: r = r >> c
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_shr_u)
		(emit-set-local r)))

(defun emit-asr-cr (c r)
	; Arithmetic shift right constant: r = r >> c (signed)
	(unless (= c 0)
		(emit-get-local r)
		(emit-byte +wasm_i64_const)
		(emit-leb128-signed c)
		(emit-byte +wasm_i64_shr_s)
		(emit-set-local r)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Memory Operations
;;;;;;;;;;;;;;;;;;;;;;;;;

; In WASM, we'll use linear memory for loads/stores
; The base register contains the address

(defun emit-cpy-ir (base offset dest)
	; Load from [base + offset] -> dest
	(emit-get-local base)
	(emit-byte +wasm_i64_load)
	(emit-leb128-unsigned 3)  ; alignment (8 bytes = 2^3)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-b (base offset dest)
	; Load byte signed
	(emit-get-local base)
	(emit-byte +wasm_i64_load8_s)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-s (base offset dest)
	; Load short signed
	(emit-get-local base)
	(emit-byte +wasm_i64_load16_s)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-i (base offset dest)
	; Load int signed
	(emit-get-local base)
	(emit-byte +wasm_i64_load32_s)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-ub (base offset dest)
	; Load byte unsigned
	(emit-get-local base)
	(emit-byte +wasm_i64_load8_u)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-us (base offset dest)
	; Load short unsigned
	(emit-get-local base)
	(emit-byte +wasm_i64_load16_u)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ir-ui (base offset dest)
	; Load int unsigned
	(emit-get-local base)
	(emit-byte +wasm_i64_load32_u)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned offset)
	(emit-set-local dest))

(defun emit-cpy-ri (src dest offset)
	; Store src -> [dest + offset]
	(emit-get-local dest)
	(emit-get-local src)
	(emit-byte +wasm_i64_store)
	(emit-leb128-unsigned 3)
	(emit-leb128-unsigned offset))

(defun emit-cpy-ri-b (src dest offset)
	; Store byte
	(emit-get-local dest)
	(emit-get-local src)
	(emit-byte +wasm_i64_store8)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned offset))

(defun emit-cpy-ri-s (src dest offset)
	; Store short
	(emit-get-local dest)
	(emit-get-local src)
	(emit-byte +wasm_i64_store16)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned offset))

(defun emit-cpy-ri-i (src dest offset)
	; Store int
	(emit-get-local dest)
	(emit-get-local src)
	(emit-byte +wasm_i64_store32)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned offset))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Load Effective Address
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-lea-i (base offset dest)
	; Load effective address: dest = base + offset
	(emit-get-local base)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed offset)
	(emit-byte +wasm_i64_add)
	(emit-set-local dest))

(defun emit-lea-d (base index dest)
	; Load effective address: dest = base + index
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-set-local dest))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Comparison and Set
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-seq-cr (c d)
	; Set if equal: d = (d == c) ? 1 : 0
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_eq)
	(emit-set-local d))

(defun emit-sne-cr (c d)
	; Set if not equal
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_ne)
	(emit-set-local d))

(defun emit-slt-cr (c d)
	; Set if less than (signed)
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_lt_s)
	(emit-set-local d))

(defun emit-sle-cr (c d)
	; Set if less or equal (signed)
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_le_s)
	(emit-set-local d))

(defun emit-sgt-cr (c d)
	; Set if greater than (signed)
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_gt_s)
	(emit-set-local d))

(defun emit-sge-cr (c d)
	; Set if greater or equal (signed)
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_ge_s)
	(emit-set-local d))

(defun emit-seq-rr (s d)
	; Set if equal
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_eq)
	(emit-set-local d))

(defun emit-sne-rr (s d)
	; Set if not equal
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_ne)
	(emit-set-local d))

(defun emit-slt-rr (s d)
	; Set if less than (signed)
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_lt_s)
	(emit-set-local d))

(defun emit-sle-rr (s d)
	; Set if less or equal (signed)
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_le_s)
	(emit-set-local d))

(defun emit-sgt-rr (s d)
	; Set if greater than (signed)
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_gt_s)
	(emit-set-local d))

(defun emit-sge-rr (s d)
	; Set if greater or equal (signed)
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_ge_s)
	(emit-set-local d))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Additional Operations
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-lnot-rr (r d)
	; Logical NOT: d = !r
	(emit-get-local r)
	(emit-byte +wasm_i64_eqz)
	(emit-set-local d))

(defun emit-ext-rr (s d)
	; Sign extend (for WASM, this is just a copy for i64)
	(emit-get-local s)
	(emit-set-local d))

(defun emit-swp-rr (s d)
	; Swap registers
	(defq temp_local 15)  ; Use a temporary local
	(emit-get-local s)
	(emit-set-local temp_local)
	(emit-get-local d)
	(emit-set-local s)
	(emit-get-local temp_local)
	(emit-set-local d))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Division (requires two destination registers)
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-div-rrr (s d_quot d_rem)
	; Division: d_quot = d_quot / s, d_rem = d_quot % s
	; WASM doesn't have a combined div/rem, so we do them separately
	(emit-get-local d_quot)
	(emit-get-local s)
	(emit-byte +wasm_i64_div_s)
	(emit-tee-local d_quot)  ; Store quotient and keep on stack
	(emit-byte +wasm_drop)   ; Drop it

	(emit-get-local d_quot)
	(emit-get-local s)
	(emit-byte +wasm_i64_rem_s)
	(emit-set-local d_rem))

(defun emit-div-rrr-u (s d_quot d_rem)
	; Unsigned division
	(emit-get-local d_quot)
	(emit-get-local s)
	(emit-byte +wasm_i64_div_u)
	(emit-tee-local d_quot)
	(emit-byte +wasm_drop)

	(emit-get-local d_quot)
	(emit-get-local s)
	(emit-byte +wasm_i64_rem_u)
	(emit-set-local d_rem))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Control Flow (Branches)
;;;;;;;;;;;;;;;;;;;;;;;;;

; WASM uses structured control flow with blocks and br_if
; We'll need to track label depths and use WASM's block/loop structures
; For now, we'll create stubs that would need more sophisticated handling

(defun emit-beq-cr (c d label depth)
	; Branch if d == c
	; This is simplified - real implementation needs block management
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_eq)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bne-cr (c d label depth)
	; Branch if d != c
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_ne)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-blt-cr (c d label depth)
	; Branch if d < c
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_lt_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-ble-cr (c d label depth)
	; Branch if d <= c
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_le_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bgt-cr (c d label depth)
	; Branch if d > c
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_gt_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bge-cr (c d label depth)
	; Branch if d >= c
	(emit-get-local d)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed c)
	(emit-byte +wasm_i64_ge_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-beq-rr (s d label depth)
	; Branch if d == s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_eq)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bne-rr (s d label depth)
	; Branch if d != s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_ne)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-blt-rr (s d label depth)
	; Branch if d < s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_lt_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-ble-rr (s d label depth)
	; Branch if d <= s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_le_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bgt-rr (s d label depth)
	; Branch if d > s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_gt_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

(defun emit-bge-rr (s d label depth)
	; Branch if d >= s
	(emit-get-local d)
	(emit-get-local s)
	(emit-byte +wasm_i64_ge_s)
	(emit-byte +wasm_br_if)
	(emit-leb128-unsigned depth))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Calls and Returns
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-call (label)
	; Direct call to a function
	; In WASM, this would be a call to a function index
	(emit-byte +wasm_call)
	(emit-leb128-unsigned label))

(defun emit-call-r (r)
	; Indirect call through register
	; WASM uses call_indirect with a type index
	(emit-get-local r)
	(emit-byte +wasm_call_indirect)
	(emit-leb128-unsigned 0)  ; Type index
	(emit-leb128-unsigned 0)) ; Table index

(defun emit-jmp (label depth)
	; Unconditional jump
	(emit-byte +wasm_br)
	(emit-leb128-unsigned depth))

(defun emit-jmp-r (r)
	; Jump through register - not directly supported in WASM
	; Would need a jump table or different approach
	(emit-call-r r))

(defun emit-ret ()
	; Return from function
	(emit-byte +wasm_return))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Stack Operations
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-push (&rest regs)
	; Push registers onto stack
	; In WASM, we would adjust the stack pointer and store
	(defq rsp (const (emit-native-reg? :rsp)))
	(when (/= 0 (length regs))
		(emit-alloc (* 8 (length regs)))
		(reach (lambda (r)
			(emit-cpy-ri r rsp (* (- (length regs) 1 (!)) 8))) regs)))

(defun emit-pop (&rest regs)
	; Pop registers from stack
	(defq rsp (const (emit-native-reg? :rsp)))
	(when (/= 0 (length regs))
		(reach (lambda (r)
			(emit-cpy-ir rsp (* (- (length regs) 1 (!)) 8) r)) regs)
		(emit-free (* 8 (length regs)))))

(defun emit-alloc (size)
	; Allocate stack space
	(emit-sub-cr (align size stack_align) (const (emit-native-reg? :rsp))))

(defun emit-free (size)
	; Free stack space
	(emit-add-cr (align size stack_align) (const (emit-native-reg? :rsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-sync (n)
	; Synchronization - nop in WASM for now
	:nil)

(defun emit-brk (n)
	; Breakpoint - could use unreachable
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Indexed Memory Access
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-dr (base index dest)
	; Load from [base + index] -> dest
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load)
	(emit-leb128-unsigned 3)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-b (base index dest)
	; Load byte from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load8_s)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-s (base index dest)
	; Load short from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load16_s)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-i (base index dest)
	; Load int from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load32_s)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-ub (base index dest)
	; Load unsigned byte from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load8_u)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-us (base index dest)
	; Load unsigned short from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load16_u)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-dr-ui (base index dest)
	; Load unsigned int from [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_i64_load32_u)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned 0)
	(emit-set-local dest))

(defun emit-cpy-rd (src base index)
	; Store src -> [base + index]
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-get-local src)
	(emit-byte +wasm_i64_store)
	(emit-leb128-unsigned 3)
	(emit-leb128-unsigned 0))

(defun emit-cpy-rd-b (src base index)
	; Store byte
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-get-local src)
	(emit-byte +wasm_i64_store8)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned 0))

(defun emit-cpy-rd-s (src base index)
	; Store short
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-get-local src)
	(emit-byte +wasm_i64_store16)
	(emit-leb128-unsigned 1)
	(emit-leb128-unsigned 0))

(defun emit-cpy-rd-i (src base index)
	; Store int
	(emit-get-local base)
	(emit-get-local index)
	(emit-byte +wasm_i64_add)
	(emit-get-local src)
	(emit-byte +wasm_i64_store32)
	(emit-leb128-unsigned 2)
	(emit-leb128-unsigned 0))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Position-independent code
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-cpy-pr (label r)
	; Load address of label into register
	; In WASM, this would be a constant
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed label)
	(emit-set-local r))

(defun emit-lea-p (label r)
	; Load effective address of label
	(emit-cpy-pr label r))

(defun emit-call-p (label)
	; Call position-independent
	(emit-call label))

(defun emit-jmp-p (label)
	; Jump position-independent
	(emit-byte +wasm_br)
	(emit-leb128-unsigned label))

(defun emit-call-i (r offset)
	; Call with base + offset
	(emit-get-local r)
	(emit-byte +wasm_i64_const)
	(emit-leb128-signed offset)
	(emit-byte +wasm_i64_add)
	(emit-byte +wasm_call_indirect)
	(emit-leb128-unsigned 0)
	(emit-leb128-unsigned 0))

(defun emit-jmp-i (r offset)
	; Jump with base + offset
	(emit-call-i r offset))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Stack initialization
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-stack-init (s f x)
	; Initialize stack for task
	(defq tk_state_size (* 8 (length stack_state)))
	(emit-sub-cr (+ tk_state_size (* 8 2)) s)
	(emit-cpy-ri f s tk_state_size)
	(emit-cpy-ri x s (+ tk_state_size 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; ABI-specific call
;;;;;;;;;;;;;;;;;;;;;;;;;

(case *abi*
(WASM32
(defun emit-call-abi (r b c n &rest x)
	; WASM ABI call - simplified
	(emit-call-i b c)))
(:t (throw (cat "Unknown ABI for CPU " *cpu* " !") *abi*)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Logical AND operation
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emit-land-rr (s d)
	; Logical AND: d = (d && s) ? 1 : 0
	; Both operands as booleans
	(emit-get-local d)
	(emit-byte +wasm_i64_eqz)
	(emit-byte +wasm_i64_eqz)  ; Convert to boolean
	(emit-get-local s)
	(emit-byte +wasm_i64_eqz)
	(emit-byte +wasm_i64_eqz)  ; Convert to boolean
	(emit-byte +wasm_i64_and)
	(emit-set-local d))

;module - export all symbols
(export-symbols (map (const first) (tolist (env))))
(env-pop)
