%include 'inc/func.inc'

;;;;;;;;;;
; forth vm
;;;;;;;;;;

	%xdefine t1 r0
	%xdefine t2 r1
	%xdefine t3 r2
	%xdefine sp r3
	%xdefine rp r4

	%macro loadsp 1-2 0
		vp_cpy [sp + (%2 * long_size)], %1
	%endmacro

	%macro storesp 1-2 0
		vp_cpy %1, [sp + (%2 * long_size)]
	%endmacro

	%macro decsp 0-1 1
		vp_sub (%1 * long_size), sp
	%endmacro

	%macro incsp 0-1 1
		vp_add (%1 * long_size), sp
	%endmacro

	%macro pushsp 1
		decsp
		storesp %1
	%endmacro

	%macro popsp 1
		loadsp %1
		incsp
	%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary building macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;format of dictionary entry flag byte
	F_IMMED equ 0x80
	F_HIDDEN equ 0x20
	F_LENMASK equ 0x1f

	;format of header
	def_structure header
		long header_llink
		long header_hlink
		ubyte header_nsize
		struct header_name, null
	def_structure_end

	;format of header backwards from code entry point
	def_structure xt
		long xt_offset
		long xt_compile
		long xt_length
		long xt_body
	def_structure_end

	%macro defword 4
		%push newword
		%strlen %%l %1
		align long_size
	dic_%3:
		dq 0				; LATEST list link
		dq 0				; hash chain link
		db %%l + %2			; flags + length byte
		db %1				; the name
		dq %3 - $			; body pointer
		dq %$code_end - %3	; code length
		dq %4 - $			; compile action word
	%3:
	%endm					; assembler code follows

	%macro defword_end 0
	%$code_end:
		%pop
	%endm

	%macro defvar 4
		defword %1, %2, %3, word_inline_comma
			vp_rel var_%3, t1
			pushsp t1
			vp_ret
		defword_end
		align long_size
	var_%3:
		dq %4
	%endm

	%macro defconst 4
		defword %1, %2, %3, word_inline_comma
			vp_cpy %4, t1
			pushsp t1
			vp_ret
		defword_end
	%endm

;;;;;;;
; forth
;;;;;;;

	fn_function forth/forth

;;;;;;;;;;;;;;
; memory words
;;;;;;;;;;;;;;

	defword "@", 0, word_fetch, word_inline_comma
		;( a -- [a] )
		loadsp t1
		vp_cpy [t1], t1
		storesp t1
		vp_ret
	defword_end

	defword "c@", 0, word_fetchbyte, word_inline_comma
		;( a -- [a].b )
		loadsp t1
		vp_cpy_ub [t1], t1
		storesp t1
		vp_ret
	defword_end

	defword "!", 0, word_store, word_inline_comma
		;( a b -- )
		popsp t1
		popsp t2
		vp_cpy t1, [t2]
		vp_ret
	defword_end

	defword "c!", 0, word_storebyte, word_inline_comma
		;( a b -- )
		popsp t1
		popsp t2
		vp_cpy_b t1, [t2]
		vp_ret
	defword_end

;;;;;;;;;;;
; alu words
;;;;;;;;;;;

	defword "+", 0, word_add, word_inline_comma
		;( a b -- (a + b))
		popsp t1
		loadsp t2
		vp_add t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "-", 0, word_sub, word_inline_comma
		;( a b -- (a - b))
		popsp t1
		loadsp t2
		vp_sub t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "*", 0, word_mull, word_inline_comma
		;( a b -- (a * b))
		popsp t1
		loadsp t2
		vp_mul t1, t2
		storesp t2
		vp_ret
	defword_end

	defword "/", 0, word_div, word_inline_comma
		;( a b -- (a / b))
		popsp t2
		loadsp t1
		vp_xor t3, t3
		vp_div t2, t3, t1
		storesp t1
		vp_ret
	defword_end

	defword "mod", 0, word_mod, word_inline_comma
		;( a b -- (a % b))
		popsp t2
		loadsp t1
		vp_xor t3, t3
		vp_div t2, t3, t1
		storesp t3
		vp_ret
	defword_end

;;;;;;;;;;;;;;;;;;;
; stack order words
;;;;;;;;;;;;;;;;;;;

	defword "dup", 0, word_dup, word_inline_comma
		;( a -- a a )
		loadsp t1
		pushsp t1
		vp_ret
	defword_end

	defword "swap", 0, word_swap, word_inline_comma
		;( a b -- b a )
		loadsp t1
		loadsp t2, 1
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "drop", 0, word_drop, word_inline_comma
		;( a -- )
		incsp
		vp_ret
	defword_end

	defword "over", 0, word_over, word_inline_comma
		;( a b -- a b a )
		loadsp t1, 1
		pushsp t1
		vp_ret
	defword_end

	defword "rot", 0, word_rot, word_inline_comma
		;( a b c -- b c a )
		loadsp t1, 2
		loadsp t2, 1
		loadsp t3
		storesp t2, 2
		storesp t3, 1
		storesp t1
		vp_ret
	defword_end

	defword "-rot", 0, word_nrot, word_inline_comma
		;( a b c -- c a b )
		loadsp t1, 2
		loadsp t2, 1
		loadsp t3
		storesp t3, 2
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "?dup", 0, word_qdup, word_inline_comma
		;( a -- a a | 0 )
		loadsp t1
		if t1, !=, 0
			pushsp t1
		endif
		vp_ret
	defword_end

	defword "!?dup", 0, word_nqdup, word_inline_comma
		;( a -- a a | 0 )
		loadsp t1
		if t1, ==, 0
			pushsp t1
		endif
		vp_ret
	defword_end

	defword "nip", 0, word_nip, word_inline_comma
		;( a b -- b )
		loadsp t1
		incsp
		storesp t1
		vp_ret
	defword_end

	defword "tuck", 0, word_tuck, word_inline_comma
		;( a b -- b a b )
		loadsp t1, 1
		loadsp t2
		decsp
		storesp t2, 2
		storesp t1, 1
		storesp t2
		vp_ret
	defword_end

	defword "pick", 0, word_pick, word_inline_comma
		;( a0 .. an n -- a0 .. an a0 )
		popsp t1
		loadsp t2, t1
		pushsp t2
		vp_ret
	defword_end

;;;;;;;;;;;;;;;;
; inlining words
;;;;;;;;;;;;;;;;

	defword "call,", 0, word_call_comma, word_call_comma
		vp_ret
	defword_end

	defword "inline,", 0, word_inline_comma, word_call_comma
		vp_ret
	defword_end

	fn_function_end
