%ifndef CODE_1234
%define CODE_1234

%include 'inc/vp.inc'

;;;;;;;;;;;;;;;;;;;;;;;
; some NASM data macros
;;;;;;;;;;;;;;;;;;;;;;;

	%assign null_size	0
	%assign byte_size	1
	%assign short_size	2
	%assign int_size	4
	%assign long_size	8

	%macro def_structure 1-2 null
		;inputs
		;%1 = structure name
		;%2 = super structure name
		%ifdef %[%1]_size
			%fatal Structure %1 allready defined !
		%endif
		%ifndef %[%2]_size
			%fatal Structure %2 not defined !
		%endif
		%define _struct_ %1
		%assign _offset_ %[%2]_size
	%endmacro

	%macro def_structure_end 0
		def_align long
		%assign %[_struct_]_size _offset_
	%endmacro

	%macro def_struct 2
		;inputs
		;%1 = field name
		;%2 = struct name
		%ifndef %[%2]_size
			%fatal Structure %2 not defined !
		%endif
		%assign %1 _offset_
		%assign _offset_ _offset_ + %[%2]_size
	%endmacro

	%macro def_long 1
		;inputs
		;%1 = field name
		def_align long
		def_struct %1, long
	%endmacro

	%macro def_int 1
		;inputs
		;%1 = field name
		def_align int
		def_struct %1, int
	%endmacro

	%macro def_short 1
		;inputs
		;%1 = field name
		def_align short
		def_struct %1, short
	%endmacro

	%macro def_byte 1
		;inputs
		;%1 = field name
		def_struct %1, byte
	%endmacro

	%macro def_align 1
		;inputs
		;%1 = alignment
		%assign _offset_ _offset_ + (%[%1]_size - 1)
		%assign _offset_ _offset_ & -(%[%1]_size)
	%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;
; some NASM codeing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro jmpif 4
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

	%macro jmpifnot 4
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

	%macro loop_start 0
		%push loop_begin
	%$loop_begin:
	%endmacro

	%macro loop_end 0
		continue
	%$loop_exit:
		%pop
	%endmacro

	%macro break 0
		vp_jmp %$loop_exit
	%endmacro

	%macro continue 0
		vp_jmp %$loop_begin
	%endmacro

	%macro continueif 3
		jmpif %1, %2, %3, %$loop_begin
	%endmacro

	%macro breakif 3
		jmpif %1, %2, %3, %$loop_exit
	%endmacro

	%macro loop_until 3
		jmpifnot %1, %2, %3, %$loop_begin
	%$loop_exit:
		%pop
	%endmacro

	%macro loop_while 3
		%push loop_begin
	%$loop_begin:
		jmpifnot %1, %2, %3, %$loop_exit
	%endmacro

	%macro if 3
		%push if
		jmpifnot %1, %2, %3, %$ifnot
	%endmacro

	%macro ifnot 3
		%push if
		jmpif %1, %2, %3, %$ifnot
	%endmacro

	%macro else 0
		%ifctx if
			%repl else
			vp_jmp %$ifend
		%$ifnot:
		%else
			%error "expected `if' before `else'"
		%endif
	%endmacro

	%macro endif 0
		%ifctx if
		%$ifnot:
			%pop
		%elifctx else
		%$ifend:
			%pop
		%else
			%error "expected `if' or `else' before `endif'"
		%endif
	%endmacro

	%macro for 4
		vp_cpy %2, %1
		loop_start
			%define %$_reg %1
			%define %$_step %4
			breakif %1, >=, %3
	%endmacro

	%macro next 0
			vp_add %$_step, %$_reg
		loop_end
	%endmacro

	%macro switch 0
		%push switch
		%assign %$_curr 1
	%endmacro

	%macro case 3
	%$loc%$_curr:
		%assign %$_curr %$_curr+1
		jmpifnot %1, %2, %3, %$loc%$_curr
	%endmacro

	%macro default 0
	%$loc%$_curr:
	%endmacro

	%macro endswitch 0
	%$loop_exit:
		%pop
	%endmacro

%endif
