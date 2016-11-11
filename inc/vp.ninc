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
	%xdefine %%t %1
	%assign _r -1
	%assign %%c 0
	%rep %0 - 1
		%rotate 1
		%ifidn %1, %%t
			%assign _r %%c
			%exitrep
		%endif
		%assign %%c %%c + 1
	%endrep
%endmacro

%macro reg_b? 1-9 r0, al, r1, cl, r2, dl, r3, bl
	%xdefine %%t %1
	%xdefine _rb %1b
	%rotate 1
	%rep (%0 - 1) / 2
		%ifidn %1, %%t
			%xdefine _rb %2
			%exitrep
		%endif
		%rotate 2
	%endrep
%endmacro

%macro reg_s? 1-9 r0, ax, r1, cx, r2, dx, r3, bx
	%xdefine %%t %1
	%xdefine _rs %1w
	%rotate 1
	%rep (%0 - 1) / 2
		%ifidn %1, %%t
			%xdefine _rs %2
			%exitrep
		%endif
		%rotate 2
	%endrep
%endmacro

%macro reg_i? 1-9 r0, eax, r1, ecx, r2, edx, r3, ebx
	%xdefine %%t %1
	%xdefine _ri %1d
	%rotate 1
	%rep (%0 - 1) / 2
		%ifidn %1, %%t
			%xdefine _ri %2
			%exitrep
		%endif
		%rotate 2
	%endrep
%endmacro

%macro vp_cpy 2
	%ifnidn %1, %2
		%ifnum %1
			%if %1 = 0
				xor %2, %2
			%else
				mov %2, %1
			%endif
		%else
			mov %2, %1
		%endif
	%endif
%endmacro

%macro vp_cpy_b 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			movsx %2, byte %1
		%endif
	%else
		;source is register
		reg_b? %1
		%xdefine %%s _rb
		reg? %2
		%if _r < 0
			;dest not register
			mov %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_ub 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			movzx %2, byte %1
		%endif
	%else
		;source is register
		reg_b? %1
		%xdefine %%s _rb
		reg? %2
		%if _r < 0
			;dest not register
			mov %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_s 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			movsx %2, word %1
		%endif
	%else
		;source is register
		reg_s? %1
		%xdefine %%s _rs
		reg? %2
		%if _r < 0
			;dest not register
			mov word %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_us 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			movzx %2, word %1
		%endif
	%else
		;source is register
		reg_s? %1
		%xdefine %%s _rs
		reg? %2
		%if _r < 0
			;dest not register
			mov word %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_i 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			movsx %2, dword %1
		%endif
	%else
		;source is register
		reg_i? %1
		%xdefine %%s _ri
		reg? %2
		%if _r < 0
			;dest not register
			mov dword %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_ui 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			reg_i? %2
			mov _ri, dword %1
		%endif
	%else
		;source is register
		reg_i? %1
		%xdefine %%s _ri
		reg? %2
		%if _r < 0
			;dest not register
			mov dword %2, %%s
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_cpy_cl 2
	reg? %2
	%if _r < 0
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
	%if _r = 2
		;high in r2
		reg? %3
		%if _r = 0
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
		%if _r = 0
			;low in r0
			vp_push r2
			vp_cpy %2, r2
			idiv %1
			vp_cpy r2, %2
			vp_pop r2
		%else
			;low not in r0
			reg? %1
			%if _r = 2
				;divider in r2
				%fatal Divide case not written
			%else
				;divider not in r2
				vp_push r0, r2
				vp_cpy %2, r2
				vp_cpy %3, r0
				idiv %1
				vp_cpy r0, %3
				vp_cpy r2, %2
				vp_pop r0, r2
			%endif
		%endif
	%endif
%endmacro

%macro vp_div_u 3
	;inputs
	;%1 = divider
	;%2:%3 = dividend high/low
	;outputs
	;%2 = remainder
	;%3 = quotient

	reg? %2
	%if _r = 2
		;high in r2
		reg? %3
		%if _r = 0
			;low in r0
			div %1
		%else
			;low not in r0
			vp_push r0
			vp_cpy %3, r0
			div %1
			vp_cpy r0, %3
			vp_pop r0
		%endif
	%else
		;high not in r2
		reg? %3
		%if _r = 0
			;low in r0
			vp_push r2
			vp_cpy %2, r2
			div %1
			vp_cpy r2, %2
			vp_pop r2
		%else
			;low not in r0
			reg? %1
			%if _r = 2
				;divider in r2
				%fatal Unsigned divide case not written
			%else
				;divider not in r2
				vp_push r0, r2
				vp_cpy %2, r2
				vp_cpy %3, r0
				div %1
				vp_cpy r0, %3
				vp_cpy r2, %2
				vp_pop r0, r2
			%endif
		%endif
	%endif
%endmacro

%macro vp_ext 2
	reg? %1
	%if _r = 0
		reg? %2
		%if _r = 2
			cqo
		%else
			if %1, <, 0
				vp_cpy -1, %2
			else
				vp_xor %2, %2
			endif
		%endif
	%else
		if %1, <, 0
			vp_cpy -1, %2
		else
			vp_xor %2, %2
		endif
	%endif
%endmacro

%macro vp_mul 2
	%ifidn %1, -1
		neg %2
	%else
		imul %2, %1
	%endif
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

%macro vp_jmpif 4
		cmp %1, %3
	%ifidn %2, ==
		je %4
	%elifidn %2, !=
		jne %4
	%elifidn %2, <
		jl %4
	%elifidn %2, >
		jg %4
	%elifidn %2, <=
		jle %4
	%elifidn %2, >=
		jge %4
	%else
		%fatal No such conditional %2 !
	%endif
%endmacro

%macro vp_jmpifnot 4
		cmp %1, %3
	%ifidn %2, ==
		jne %4
	%elifidn %2, !=
		je %4
	%elifidn %2, <
		jge %4
	%elifidn %2, >
		jle %4
	%elifidn %2, <=
		jg %4
	%elifidn %2, >=
		jl %4
	%else
		%fatal No such conditional %2 !
	%endif
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
	add %1, 1
%endmacro

%macro vp_dec 1
	sub %1, 1
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
	reg? %1
	%if _r < 0
		;source is not reg
		shr %2, %1
	%elif _r = 1
		;source is r1
		shr %2, cl
	%else
		;source is not r1
		reg? %2
		%if _r < 0
			;dest is not reg
			%fatal Not valid VP mode
		%elif _r = 1
			;dest is r1
			vp_push %1
			vp_xchg %1, %2
			shr %1, cl
			vp_cpy %1, %2
			vp_pop %1
		%else
			;dest is not r1
			vp_xchg %1, r1
			shr %2, cl
			vp_xchg %1, r1
		%endif
	%endif
%endmacro

%macro vp_asr 2
	reg? %1
	%if _r < 0
		;source is not reg
		sar %2, %1
	%elif _r = 1
		;source is r1
		sar %2, cl
	%else
		;source is not r1
		reg? %2
		%if _r < 0
			;dest is not reg
			%fatal Not valid VP mode
		%elif _r = 1
			;dest is r1
			vp_push %1
			vp_xchg %1, %2
			sar %1, cl
			vp_cpy %1, %2
			vp_pop %1
		%else
			;dest is not r1
			vp_xchg %1, r1
			sar %2, cl
			vp_xchg %1, r1
		%endif
	%endif
%endmacro

%macro vp_shl 2
	reg? %1
	%if _r < 0
		;source is not reg
		shl %2, %1
	%elif _r = 1
		;source is r1
		shl %2, cl
	%else
		;source is not r1
		reg? %2
		%if _r < 0
			;dest is not reg
			%fatal Not valid VP mode
		%elif _r = 1
			;dest is r1
			vp_push %1
			vp_xchg %1, %2
			shl %1, cl
			vp_cpy %1, %2
			vp_pop %1
		%else
			;dest is not r1
			vp_xchg %1, r1
			shl %2, cl
			vp_xchg %1, r1
		%endif
	%endif
%endmacro

%macro vp_cpy_rel 2
	reg? %1
	%if _r < 0
		;source not register
		reg? %2
		%if _r < 0
			;dest not register
			%fatal Not valid VP mode
		%else
			;dest is register
			mov %2, [rel %1]
		%endif
	%else
		;source is register
		reg? %2
		%if _r < 0
			;dest not register
			mov [rel %2], %1
		%else
			;dest is register
			%fatal Not valid VP mode
		%endif
	%endif
%endmacro

%macro vp_call_rel 1
	call [rel %1]
%endmacro

%macro vp_jmp_rel 1
	jmp [rel %1]
%endmacro

%macro vp_rel 2
	lea %2, [rel %1]
%endmacro

%endif
