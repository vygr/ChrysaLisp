%ifndef CODE_1234
%define CODE_1234

%include 'inc/vp.inc'

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

	null_size	equ 0
	byte_size	equ 1
	short_size	equ 2
	int_size	equ 4
	long_size	equ 8

	%macro def_structure 1-2 null
		;inputs
		;%1 = structure name
		;%2 = super structure name
		%xdefine _struct_ %1
		%assign _offset_ %2_size
	%endmacro

	%macro def_structure_end 0
		def_align long
		%[_struct_]_size equ _offset_
	%endmacro

	%macro def_struct 2
		;inputs
		;%1 = field name
		;%2 = struct name
		%1 equ _offset_
		%assign _offset_ _offset_ + %2_size
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
		%assign _offset_ _offset_ + (%1_size - 1)
		%assign _offset_ _offset_ & -(%1_size)
	%endmacro

	%macro def_local 0-2 local, null
		def_structure %1, %2
		%xdefine _local_ %1
	%endmacro

	%macro def_local_long 1
		;%1 name
		def_long %[_local_]_%1
		%xdefine .%1 [r4 + %[_local_]_%1]
	%endmacro

	%macro def_local_struct 2
		;%1 name
		;%2 size
		def_struct %[_local_]_%1, %2
		%xdefine .%1 [r4 + %[_local_]_%1]
	%endmacro

	%macro def_local_end 0
		def_structure_end
	%endmacro

;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;
; paramater handling
;;;;;;;;;;;;;;;;;;;;

	%macro set_src 0-*
		;%1... = paramaters
		%assign _src_total 0
		%rep %0
			%xdefine _src_%[_src_total] %1
			%assign _src_total _src_total + 1
			%rotate 1
		%endrep
	%endmacro

	%macro set_dst 0-*
		;%1... = paramaters
		%assign _dst_total 0
		%rep %0
			%xdefine _dst_%[_dst_total] %1
			%assign _dst_total _dst_total + 1
			%rotate 1
		%endrep
	%endmacro

	%macro map_print 0
		%warning src => dst
		%assign %%i 0
		%rep _src_total
			%warning map entry %%i: _src_%[%%i] => _dst_%[%%i]
			%assign %%i %%i + 1
		%endrep
	%endmacro

	%macro map_rotate 2
		;%1 = dst index
		;%2 = src index
		%xdefine %%s _src_%[%2]
		%xdefine %%d _dst_%[%2]
		%assign %%j %2
		%rep %2 - %1
			%assign %%i %%j - 1
			%xdefine _src_%[%%j] _src_%[%%i]
			%xdefine _dst_%[%%j] _dst_%[%%i]
			%assign %%j %%i
		%endrep
		%xdefine _src_%1 %%s
		%xdefine _dst_%1 %%d
	%endmacro

	%macro map_remove_ignored 0
		%assign %%i 0
		%assign %%j 0
		%rep _dst_total
			%ifnidn _dst_%[%%j], _
				%ifnidn _src_%[%%j], _dst_%[%%j]
					%if %%i != %%j
						%xdefine _src_%[%%i] _src_%[%%j]
						%xdefine _dst_%[%%i] _dst_%[%%j]
					%endif
					%assign %%i %%i + 1
				%endif
			%endif
			%assign %%j %%j + 1
			%if %%j = _dst_total
				%exitrep
			%endif
		%endrep
		%assign _src_total %%i
		%assign _dst_total %%i
	%endmacro

	%macro sub_string 2
		;%1 = param to find
		;%2 = param to search
		%strlen %%l1 %1
		%strlen %%l2 %2
		%assign _pos 0
		%if %%l1 <= %%l2
			%assign _pos %%l2 + 1 - %%l1
			%rep _pos
				%substr %%ss2 %2 _pos, %%l1
				%ifidn %%ss2, %1
					%exitrep
				%else
					%assign _pos _pos - 1
				%endif
			%endrep
		%endif
	%endmacro

	%macro sub_token 2
		;%1 = param to find
		;%2 = param to search
		%defstr %%s1 %1
		%defstr %%s2 %2
		sub_string %%s1, %%s2
	%endmacro

	%macro find_later_src 1
		;%1 = index of dst
		%assign _idx -1
		%assign %%i _src_total - 1
		%rep %%i - %1
			%ifnstr _src_%[%%i]
				%ifnnum _src_%[%%i]
					sub_token _dst_%1, _src_%[%%i]
					%if _pos != 0
						%assign _idx %%i
						%exitrep
					%endif
				%endif
			%endif
			%assign %%i %%i - 1
		%endrep
	%endmacro

	%macro map_topology_sort 0
		%assign %%c 1000
		%rep %%c
			%assign %%i 0
			%rep _dst_total
				find_later_src %%i
				%if _idx > %%i
					map_rotate %%i, _idx
					%exitrep
				%else
					%assign %%i %%i + 1
				%endif
			%endrep
			%if %%i = _dst_total
				%exitrep
			%endif
			%assign %%c %%c - 1
		%endrep
		%if %%c = 0
			map_print
			%error Copy cycle detected !
		%endif
	%endmacro

	%macro map_src_to_dst 0
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		map_topology_sort
		%assign %%i 0
		%rep _src_total
			%ifstr _src_%[%%i]
				;string
				fn_string _src_%[%%i], _dst_%[%%i]
			%else
				%defstr %%s _src_%[%%i]
				%substr %%ss %%s 1, 1
				%ifidn %%ss, '@'
					;bind function
					%substr %%ss %%s 2, -1
					%deftok %%p %%ss
					fn_bind %%p, _dst_%[%%i]
				%elifidn %%ss, ':'
					;address of
					%substr %%ss %%s 2, -1
					%deftok %%p %%ss
					vp_lea %%p, _dst_%[%%i]
				%elifidn %%ss, '$'
					;label address
					%substr %%ss %%s 2, -1
					%deftok %%p %%ss
					vp_rel %%p, _dst_%[%%i]
				%else
					;just a copy
					vp_cpy _src_%[%%i], _dst_%[%%i]
				%endif
			%endif
			%assign %%i %%i + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;
; expression compilation
;;;;;;;;;;;;;;;;;;;;;;;;

	%macro set_reg 2
		%xdefine _reg_%[%1] %2
	%endmacro

	%macro get_reg 1
		%xdefine _reg _reg_%[%1]
	%endmacro

	%macro push_reg 0
		%assign _reg_sp _reg_sp + 1
		%if _reg_sp = _reg_total
			%error Register stack overflow !
		%endif
	%endmacro

	%macro pop_reg 0
		%assign _reg_sp _reg_sp - 1
		%if _reg_sp = -1
			%error Register stack underflow !
		%endif
	%endmacro

	%macro reset_reg_stack 0
		%assign _reg_sp 0
		%assign _reg_total 0
	%endmacro

	%macro add_reg_stack 1
		%assign %%n 0
		%rep _reg_total
			get_reg %%n
			%ifidn _reg, %1
				%exitrep
			%else
				%assign %%n %%n + 1
			%endif
		%endrep
		%if %%n = _reg_total
			set_reg _reg_total, %1
			%assign _reg_total _reg_total + 1
		%endif
	%endmacro

	%macro fill_reg_stack 0
		add_reg_stack r0
		add_reg_stack r1
		add_reg_stack r2
		add_reg_stack r3
		add_reg_stack r5
		add_reg_stack r6
		add_reg_stack r7
		add_reg_stack r8
		add_reg_stack r9
		add_reg_stack r10
		add_reg_stack r11
		add_reg_stack r12
		add_reg_stack r13
		add_reg_stack r14
		add_reg_stack r15
	%endmacro

	%macro print_reg_stack 0
		%assign %%n 0
		%rep _reg_sp
			get_reg %%n
			%warning param %%n: _reg
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro get_op_presidence 1
		%ifidn %1, *
			%assign _op 3
		%elifidn %1, /
			%assign _op 3
		%elifidn %1, %
			%assign _op 3
		%elifidn %1, +
			%assign _op 4
		%elifidn %1, -
			%assign _op 4
		%elifidn %1, &
			%assign _op 8
		%elifidn %1, ^
			%assign _op 9
		%elifidn %1, |
			%assign _op 10
		%elifidn %1, (
			%assign _op 100
		%else
			%error Unknown operator %1
		%endif
	%endmacro

	%macro push_token 2
		%deftok %%t %1
		%xdefine _token_%[_token_total] %%t
		%assign _token_type_%[_token_total] %2
		%assign _token_total _token_total + 1
	%endmacro

	%macro set_token_list 1
		%defstr %%s %1
		%strlen %%l %%s
		%assign _token_total 0
		%defstr %%p
		%assign %%m -1
		%assign %%i 1
		%rep %%l
			%substr %%ss %%s %%i, 1
			sub_string %%ss, '[":@$] ()+-*/%&^|'
			%if %%m = -1
				;building a ...
				%if _pos = 0
					%strcat %%p %%p %%ss
				%elif _pos = 7
					%ifnidn %%p, ''
						push_token %%p, %%m
						%defstr %%p
					%endif
				%elif _pos > 7
					push_token %%ss, 0
				%else
					%assign %%m _pos
				%endif
			%elif %%m = 1
				;building a [...]
				%strcat %%p %%p %%ss
				%if _pos = 6
					%strcat %%p '[' %%p
					push_token %%p, %%m
					%defstr %%p
					%assign %%m -1
				%endif
			%elif %%m = 2
				;building a "..."
				%strcat %%p %%p %%ss
				%if _pos = 2
					%strcat %%p '"' %%p
					push_token %%p, %%m
					%defstr %%p
					%assign %%m -1
				%endif
			%elif %%m = 3
				;building :[...]
				%strcat %%p %%p %%ss
				%if _pos = 6
					push_token %%p, %%m
					%defstr %%p
					%assign %%m -1
				%endif
			%else
				;building @... or $...
				%strcat %%p %%p %%ss
				%if _pos = 7
					push_token %%p, %%m
					%defstr %%p
					%assign %%m -1
				%endif
			%endif
			%assign %%i %%i + 1
		%endrep
		%ifnidn %%p, ''
			push_token %%p, %%m
		%endif
	%endmacro

	%macro print_token_list 0
		%assign %%n 0
		%rep _token_total
			%warning token %%n: _token_type_%[%%n] : _token_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro push_rpn 2
		%xdefine _rpn_%[_rpn_total] %1
		%assign _rpn_type_%[_rpn_total] %2
		%assign _rpn_total _rpn_total + 1
	%endmacro

	%macro token_to_rpn 0
		%assign _rpn_total 0
		%assign %%o 0
		%assign %%n 0
		%rep _token_total
			%if _token_type_%[%%n] = 0
				%ifidn _token_%[%%n], (
					%xdefine _op_%[%%o] _token_%[%%n]
					%assign %%o %%o + 1
				%elifidn _token_%[%%n], )
					%rep %%o
						%assign %%o %%o - 1
						%ifidn _op_%[%%o], (
							%exitrep
						%else
							push_rpn _op_%[%%o], 0
						%endif
					%endrep
				%else
					get_op_presidence _token_%[%%n]
					%assign %%t _op
					%rep %%o
						%assign %%o %%o - 1
						get_op_presidence _op_%[%%o]
						%if %%t >= _op
							push_rpn _op_%[%%o], 0
						%else
							%assign %%o %%o + 1
							%exitrep
						%endif
					%endrep
					%xdefine _op_%[%%o] _token_%[%%n]
					%assign %%o %%o + 1
				%endif
			%else
				push_rpn _token_%[%%n], _token_type_%[%%n]
			%endif
			%assign %%n %%n + 1
		%endrep
		%rep %%o
			%assign %%o %%o - 1
			push_rpn _op_%[%%o], 0
		%endrep
	%endmacro

	%macro print_rpn_list 0
		%assign %%n 0
		%rep _rpn_total
			%warning rpn token %%n: _rpn_type_%[%%n], _rpn_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro emit_rpn_list 0
		%assign %%n 0
		%rep _rpn_total
			%if _rpn_type_%[%%n] = 0
				pop_reg
				get_reg _reg_sp
				%xdefine %%r1 _reg
				pop_reg
				get_reg _reg_sp
				%xdefine %%r0 _reg
				%ifidn _rpn_%[%%n], +
					%warning vp_add %%r1, %%r0
					vp_add %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], -
					%warning vp_sub %%r1, %%r0
					vp_sub %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], *
					%warning vp_mul %%r1, %%r0
					vp_mul %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], &
					%warning vp_and %%r1, %%r0
					vp_and %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], ^
					%warning vp_xor %%r1, %%r0
					vp_xor %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], |
					%warning vp_or %%r1, %%r0
					vp_or %%r1, %%r0
					push_reg
				%elifidn _rpn_%[%%n], /
					push_reg
					push_reg
					get_reg _reg_sp
					%warning vp_xor _reg, _reg
					%warning vp_div %%r1, _reg, %%r0
					vp_xor _reg, _reg
					vp_div %%r1, _reg, %%r0
					pop_reg
				%elifidn _rpn_%[%%n], %
					push_reg
					push_reg
					get_reg _reg_sp
					%warning vp_xor _reg, _reg
					%warning vp_div %%r1, _reg, %%r0
					%warning vp_cpy _reg, %%r0
					vp_xor _reg, _reg
					vp_div %%r1, _reg, %%r0
					vp_cpy _reg, %%r0
					pop_reg
				%else
					%error Unknown operator _rpn_%[%%n]
				%endif
			%elif _rpn_type_%[%%n] = 2
				;"..."
				get_reg _reg_sp
				push_reg
				%warning fn_string _rpn_%[%%n], _reg
				fn_string _rpn_%[%%n], _reg
			%elif _rpn_type_%[%%n] = 3
				;:[...]
				get_reg _reg_sp
				push_reg
				%warning vp_lea _rpn_%[%%n], _reg
				vp_lea _rpn_%[%%n], _reg
			%elif _rpn_type_%[%%n] = 4
				;@...
				get_reg _reg_sp
				push_reg
				%warning fn_bind _rpn_%[%%n], _reg
				fn_bind _rpn_%[%%n], _reg
			%elif _rpn_type_%[%%n] = 5
				;$...
				get_reg _reg_sp
				push_reg
				%warning vp_rel _rpn_%[%%n], _reg
				vp_rel _rpn_%[%%n], _reg
			%else
				;...
				get_reg _reg_sp
				push_reg
				%ifnidn _rpn_%[%%n], _reg
					%warning vp_cpy _rpn_%[%%n], _reg
					vp_cpy _rpn_%[%%n], _reg
				%endif
			%endif
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro assign 2
		%warning ---------------
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		map_topology_sort
		reset_reg_stack
		%assign %%n 0
		%rep _dst_total
			reg? _dst_%[%%n]
			%if _reg = -1
				%error _dst_%[%%n] not a register !
			%endif
			add_reg_stack _dst_%[%%n]
			%assign %%n %%n + 1
		%endrep
		fill_reg_stack
		%assign %%n 0
		%rep _src_total
			set_token_list _src_%[%%n]
			token_to_rpn
			emit_rpn_list
			%assign %%n %%n + 1
		%endrep
	%endmacro

%endif
