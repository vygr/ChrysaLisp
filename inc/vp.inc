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

	%macro reg? 1-17 r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15
		%define _test %1
		%assign _reg -1
		%assign _cnt 0
		%rep %0 - 1
			%rotate 1
			%ifidn %1, _test
				%assign _reg _cnt
				%exitrep
			%endif
			%assign _cnt _cnt + 1
		%endrep
	%endmacro

	%macro reg_b? 1-9 r0, al, r1, cl, r2, dl, r3, bl
		%define _test %1
		%define _reg_b %1l
		%rep %0 - 1
			%rotate 1
			%ifidn %1, _test
				%define _reg_b %2
				%exitrep
			%endif
		%endrep
	%endmacro

	%macro reg_w? 1-9 r0, ax, r1, cx, r2, dx, r3, bx
		%define _test %1
		%define _reg_w %1x
		%rep %0 - 1
			%rotate 1
			%ifidn %1, _test
				%define _reg_w %2
				%exitrep
			%endif
		%endrep
	%endmacro

	%macro vp_cpy 2
		%ifnidn %1, %2
			mov %2, %1
		%endif
	%endmacro

	%macro vp_cpy_b 2
		reg? %1
		%if _reg < 0
			;source not register
			reg? %2
			%if _reg < 0
				;dest not register
				%fatal Not valid VP mode
			%else
				;dest is register
				reg_b? %2
				mov _reg_b, byte %1
			%endif
		%else
			;source is register
			reg_b? %1
			%define _src_b _reg_b
			reg? %2
			%if _reg < 0
				;dest not register
				mov byte %2, _src_b
			%else
				;dest is register
				reg_b? %2
				mov _reg_b, _src_b
			%endif
		%endif
	%endmacro

	%macro vp_cpy_w 2
		reg? %1
		%if _reg < 0
			;source not register
			reg? %2
			%if _reg < 0
				;dest not register
				%fatal Not valid VP mode
			%else
				;dest is register
				reg_w? %2
				mov _reg_w, word %1
			%endif
		%else
			;source is register
			reg_w? %1
			%define _src_w _reg_w
			reg? %2
			%if _reg < 0
				;dest not register
				mov word %2, _src_w
			%else
				;dest is register
				reg_w? %2
				mov _reg_w, _src_w
			%endif
		%endif
	%endmacro

	%macro vp_xchg 2
		%ifnidn %1, %2
			xchg %1, %2
		%endif
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
		%ifnidn %1, %2
			and %2, %1
		%endif
	%endmacro

	%macro vp_or 2
		%ifnidn %1, %2
			or %2, %1
		%endif
	%endmacro

	%macro vp_xor 2
		xor %2, %1
	%endmacro

	%macro vp_shr 2
		shr %2, %1
	%endmacro

	%macro vp_asr 2
		sar %2, %1
	%endmacro

	%macro vp_shl 2
		shl %2, %1
	%endmacro

%endif
