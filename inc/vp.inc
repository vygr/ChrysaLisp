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
		%define _reg_b %1b
		%rotate 1
		%rep (%0 - 1) / 2
			%ifidn %1, _test
				%define _reg_b %2
				%exitrep
			%endif
			%rotate 2
		%endrep
	%endmacro

	%macro reg_s? 1-9 r0, ax, r1, cx, r2, dx, r3, bx
		%define _test %1
		%define _reg_s %1w
		%rotate 1
		%rep (%0 - 1) / 2
			%ifidn %1, _test
				%define _reg_s %2
				%exitrep
			%endif
			%rotate 2
		%endrep
	%endmacro

	%macro reg_i? 1-9 r0, eax, r1, ecx, r2, edx, r3, ebx
		%define _test %1
		%define _reg_i %1d
		%rotate 1
		%rep (%0 - 1) / 2
			%ifidn %1, _test
				%define _reg_i %2
				%exitrep
			%endif
			%rotate 2
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

	%macro vp_cpy_s 2
		reg? %1
		%if _reg < 0
			;source not register
			reg? %2
			%if _reg < 0
				;dest not register
				%fatal Not valid VP mode
			%else
				;dest is register
				reg_s? %2
				mov _reg_s, word %1
			%endif
		%else
			;source is register
			reg_s? %1
			%define _src_s _reg_s
			reg? %2
			%if _reg < 0
				;dest not register
				mov word %2, _src_s
			%else
				;dest is register
				reg_s? %2
				mov _reg_s, _src_s
			%endif
		%endif
	%endmacro

	%macro vp_cpy_i 2
		reg? %1
		%if _reg < 0
			;source not register
			reg? %2
			%if _reg < 0
				;dest not register
				%fatal Not valid VP mode
			%else
				;dest is register
				reg_i? %2
				mov _reg_i, dword %1
			%endif
		%else
			;source is register
			reg_i? %1
			%define _src_i _reg_i
			reg? %2
			%if _reg < 0
				;dest not register
				mov dword %2, _src_i
			%else
				;dest is register
				reg_i? %2
				mov _reg_i, _src_i
			%endif
		%endif
	%endmacro

	%macro vp_cpy_cl 2
		reg? %2
		%if _reg < 0
			;dest not register
			mov qword %2, %1
		%else
			;dest is register
			mov %2, %1
		%endif
	%endmacro

	%macro vp_xchg 2
		%ifnidn %1, %2
			xchg %1, %2
		%endif
	%endmacro

	%macro vp_div 3
		;inputs
		;%1 = divider
		;%2:%3 = dividend high/low
		;outputs
		;%2 = remainder
		;%3 = quotient

		reg? %2
		%if _reg = 2
			;high in r2
			reg? %3
			%if _reg = 0
				;low in r0
				idiv %1
			%else
				;low not in r0
				vp_push r0
				vp_cpy %3, r0
				idiv %1
				vp_cpy r0, %3
				vp_pop r0
			%endif
		%else
			;high not in r2
			reg? %3
			%if _reg = 0
				;low in r0
				vp_push r2
				vp_cpy %2, r2
				idiv %1
				vp_cpy r2, %2
				vp_pop r2
			%else
				;low not in r0
				vp_push r0, r2
				vp_cpy %2, r2
				vp_cpy %3, r0
				idiv %1
				vp_cpy r0, %3
				vp_cpy r2, %2
				vp_pop r0, r2
			%endif
		%endif
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

	%macro vp_adr 2
		lea %2, [rel %1]
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
