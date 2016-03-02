%ifndef VP_1234
	%define VP_1234

;;;;;;;;;;;;;;;;
; vp code macros
;;;;;;;;;;;;;;;;

	%use altreg
	;r0 rax
	;r1 rcx
	;r2 rdx
	;r3 rbx
	;r4 rsp
	;r5 rbp
	;r6 rsi
	;r7 rdi

	%macro vp_xchg 2
		xchg %1, %2
	%endmacro

	%macro vp_div 1
		idiv %1
	%endmacro

	%macro vp_mul 2
		imul %2, %1
	%endmacro

	%macro vp_syscall 0
		syscall
	%endmacro

	%macro vp_call 1
		call %1
	%endmacro

	%macro vp_ret 0
		ret
	%endmacro

	%macro vp_jmp 1
		jmp %1
	%endmacro

	%macro vp_push 1-*
		%rep %0
			push %1
			%rotate 1
		%endrep
	%endmacro

	%macro vp_pop 1-*
		%rep %0
			%rotate -1
			pop %1
		%endrep
	%endmacro

	%macro vp_lea 2
		lea %2, %1
	%endmacro

	%macro vp_cpy 2
		mov %2, %1
	%endmacro

	%macro vp_inc 1
		inc %1
	%endmacro

	%macro vp_dec 1
		dec %1
	%endmacro

	%macro vp_add 2
		add %2, %1
	%endmacro

	%macro vp_sub 2
		sub %2, %1
	%endmacro

	%macro vp_cmp 2
		sub %2, %1
	%endmacro

	%macro vp_and 2
		and %2, %1
	%endmacro

	%macro vp_or 2
		or %2, %1
	%endmacro

	%macro vp_xor 2
		xor %2, %1
	%endmacro

	%macro vp_shr 2
		shr %2, %1
	%endmacro

%endif
