%ifndef CODE_1234
%define CODE_1234

%include 'inc/vp.inc'

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

	%macro switch 0
		%push scope
		%assign %$_cnt 0
	%$_begin:
	%endmacro

	%macro default 0
	%$_loc_%$_cnt:
		%assign %$_cnt %$_cnt + 1
	%endmacro

	%macro endswitch 0
	%$_end:
		default
		%pop
	%endmacro

	%macro case 1
		default
		eval {%1}, {r0}
		jmpifnot r0, !=, 0, %$_loc_%$_cnt
	%endmacro

	%macro case 3
		default
		jmpifnot %1, %2, %3, %$_loc_%$_cnt
	%endmacro

	%macro casenot 1
		default
		eval {%1}, {r0}
		jmpif r0, !=, 0, %$_loc_%$_cnt
	%endmacro

	%macro casenot 3
		default
		jmpif %1, %2, %3, %$_loc_%$_cnt
	%endmacro

	%macro break 0
		jmp %$_end
	%endmacro

	%macro breakif 3
		jmpif %1, %2, %3, %$_end
	%endmacro

	%macro breakif 1
		eval {%1}, {r0}
		breakif r0, !=, 0
	%endmacro

	%macro breakifnot 3
		jmpifnot %1, %2, %3, %$_end
	%endmacro

	%macro breakifnot 1
		eval {%1}, {r0}
		breakifnot r0, !=, 0
	%endmacro

	%macro continue 0
		jmp %$_begin
	%endmacro

	%macro continueif 3
		jmpif %1, %2, %3, %$_begin
	%endmacro

	%macro continueif 1
		eval {%1}, {r0}
		continueif r0, !=, 0
	%endmacro

	%macro continueifnot 3
		jmpifnot %1, %2, %3, %$_begin
	%endmacro

	%macro continueifnot 1
		eval {%1}, {r0}
		continueifnot r0, !=, 0
	%endmacro

	%macro loop_start 0
		switch
	%endmacro

	%macro loop_while 3
		switch
		breakifnot %1, %2, %3
	%endmacro

	%macro loop_while 1
		switch
		breakifnot {%1}
	%endmacro

	%macro loop_end 0
		continue
		endswitch
	%endmacro

	%macro loop_until 1
		continueifnot {%1}
		endswitch
	%endmacro

	%macro loop_until 3
		continueifnot %1, %2, %3
		endswitch
	%endmacro

	%macro if 1
		switch
		case {%1}
	%endmacro

	%macro if 3
		switch
		case %1, %2, %3
	%endmacro

	%macro ifnot 1
		switch
		casenot {%1}
	%endmacro

	%macro ifnot 3
		switch
		casenot %1, %2, %3
	%endmacro

	%macro elseif 1
		break
		case {%1}
	%endmacro

	%macro elseif 3
		break
		case %1, %2, %3
	%endmacro

	%macro else 0
		break
		default
	%endmacro

	%macro endif 0
		endswitch
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

;;;;;;;;;;;;;;;;;
; data structures
;;;;;;;;;;;;;;;;;

	null_size	equ 0
	byte_size	equ 1
	short_size	equ 2
	int_size	equ 4
	long_size	equ 8
	ptr_size	equ 8

	%macro def_structure 1-2 null
		;inputs
		;%1 = structure name
		;%2 = super structure name
		%xdefine _struct_ %1
		%assign _offset_ %2_size
	%endmacro

	%macro def_structure_end 0
		local_align long
		%[_struct_]_size equ _offset_
		%undef _struct_
		%undef _offset_
	%endmacro

;;;;;;;;;;;;;;;;;;;;;
; variable allocation
;;;;;;;;;;;;;;;;;;;;;

	%assign _var_sp 0

	%macro local_align 0-1 long
		;inputs
		;%1 = alignment
		%ifdef _struct_
			%assign _offset_ _offset_ + (%1_size - 1)
			%assign _offset_ _offset_ & -(%1_size)
		%else
			%assign _var_sp _var_sp + (%1_size - 1)
			%assign _var_sp _var_sp & -(%1_size)
		%endif
	%endmacro

	%macro struct 2-3 ''
		;inputs
		;%1 = var name
		;%2 = struct name
		;%3 = type 'pppi'
		%ifdef _struct_
			%1 equ _offset_
			%xdefine %1_t %3
			%assign _offset_ _offset_ + %2_size
		%else
			def_sym %1, _sym_var, _var_sp, 0, %3
			%assign _var_sp _var_sp + %2_size
		%endif
	%endmacro

	%macro long 1
		;inputs
		;%1 = var name
		local_align long
		struct %1, long, 'l'
	%endmacro

	%macro ulong 1
		;inputs
		;%1 = var name
		local_align long
		struct %1, long, 'L'
	%endmacro

	%macro int 1
		;inputs
		;%1 = var name
		local_align int
		struct %1, int, 'l'
	%endmacro

	%macro uint 1
		;inputs
		;%1 = var name
		local_align int
		struct %1, int, 'I'
	%endmacro

	%macro short 1
		;inputs
		;%1 = var name
		local_align short
		struct %1, short, 's'
	%endmacro

	%macro ushort 1
		;inputs
		;%1 = var name
		local_align short
		struct %1, short, 'S'
	%endmacro

	%macro byte 1
		;inputs
		;%1 = var name
		struct %1, byte, 'b'
	%endmacro

	%macro ubyte 1
		;inputs
		;%1 = var name
		struct %1, byte, 'B'
	%endmacro

	%macro pubyte 1
		;inputs
		;%1 = var name
		struct %1, ptr, 'pB'
	%endmacro

	%macro pulong 1
		;inputs
		;%1 = var name
		struct %1, ptr, 'pL'
	%endmacro

;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

	%assign _sym_sp 0
	%assign _scope_sp 0
	_sym_op		equ 0
	_sym_const	equ 1
	_sym_var	equ 2

	%macro push_scope 0
		local_align
		%assign _scope_sym_%[_scope_sp] _sym_sp
		%assign _scope_var_%[_scope_sp] _var_sp
		%assign _scope_sp _scope_sp + 1
		%if _var_sp != 0
			%warning vp_sub _var_sp, r4
			vp_sub _var_sp, r4
			%assign _var_sp 0
		%endif
	%endmacro

	%macro pop_scope 0
		%assign _scope_sp _scope_sp - 1
		%assign _sym_sp _scope_sym_%[_scope_sp]
		%assign _var_sp _scope_var_%[_scope_sp]
		%if _var_sp != 0
			%warning vp_add _var_sp, r4
			vp_add _var_sp, r4
			%assign _var_sp 0
		%endif
		%assign %%s _scope_sp - 1
		%if %%s != -1
			%assign _sym_sp _scope_sym_%[%%s]
		%endif
	%endmacro

	%macro get_scope_offset 1
		;%1 scope to accsess
		%assign _scope_offset 0
		%if %1 >= _scope_sp
			%fatal Symbol not in scope !
		%endif
		%assign %%n _scope_sp - 1
		%rep %%n - %1
			%assign _scope_offset _scope_offset + _scope_var_%[%%n]
			%assign %%n %%n - 1
		%endrep
	%endmacro

	%macro def_sym 3-5 0, ''
		;%1 name
		;%2 type
		;%3 value1
		;%4 value2
		;%5 aux data
		%assign %%s _scope_sp -1
		%assign %%n _scope_sym_%[%%s]
		%rep _sym_sp - %%n
			%ifidn _sym_name_%[%%n], %1
				%fatal Symbol %1 redefined !
			%endif
			%assign %%n %%n + 1
		%endrep
		%assign _sym_scope_%[%%n] _scope_sp
		%xdefine _sym_name_%[%%n] %1
		%assign _sym_type_%[%%n] %2
		%assign _sym_value1_%[%%n] %3
		%assign _sym_value2_%[%%n] %4
		%xdefine _sym_aux_%[%%n] %5
		%assign _sym_sp _sym_sp + 1
	%endmacro

	%macro get_sym 1
		;%1 name
		%assign _sym _sym_sp - 1
		%rep _sym_sp
			%ifidn _sym_name_%[_sym], %1
				%exitrep
			%else
				%assign _sym _sym - 1
			%endif
		%endrep
	%endmacro

	%macro print_sym 0
		%assign %%n 0
		%rep _sym_sp
			%warning sc: _sym_scope_%[%%n] \
					t: _sym_type_%[%%n] \
					n: _sym_name_%[%%n] \
					v1: _sym_value1_%[%%n] \
					v2: _sym_value2_%[%%n] \
					x: _sym_aux_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro const 2
		;%1 name
		;%2 value
		def_sym %1, _sym_const, %2
	%endmacro

	%macro operator 2-4 0, compile_null
		;%1 name
		;%2 precidence
		;%3 asociativity
		;%4 compile macro
		def_sym %1, _sym_op, %2 * 2, %3, %4
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
				%elifidn %%ss, '&'
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

;;;;;;;;;;;;;;
; token parser
;;;;;;;;;;;;;;

	%macro push_token 2
		%deftok %%t %1
		%xdefine _token_%[_token_sp] %%t
		%assign _token_type_%[_token_sp] %2
		%assign _token_sp _token_sp + 1
	%endmacro

	%macro set_token_list 1
		%defstr %%s %1
		%strlen %%l %%s
		%assign _token_sp 0
		%defstr %%p
		%defstr %%c
		%assign %%m -1
		%assign %%u 1
		%assign %%i 1
		%rep %%l * 2
			%ifnidn %%c, ''
				sub_string %%c, '@$"-*~!+.?^</%&|=> )(]['
				%if %%m = -1
					;scanning
					%ifnidn %%c, ' '
						%if _pos >= 20
							;)(
							push_token %%c, _pos
							%assign %%u (_pos - 20) & 1
						%elif (_pos = 0 || _pos >= 3)
							;"-*~!+.?^</%&|=> or symbol
							%strcat %%p %%p %%c
							%assign %%m _pos
						%else
							;@$
							%assign %%m _pos
						%endif
					%endif
					%defstr %%c
				%elif %%m > 3
					;op mode
					%if _pos >= 20 || _pos < 12
						;space, )( or not </%&|=>
						%if %%u = 1
							;unary operator
							sub_string %%p, '-*&~!'
							%if _pos != 0
								%substr %%o '_#:~!' _pos, 1
								push_token %%o, %%m
							%else
								%fatal Unknown unary operator %%p !
							%endif
						%else
							;binary operator
							push_token %%p, %%m
							%assign %%u 1
						%endif
						%defstr %%p
						%assign %%m -1
					%else
						%strcat %%p %%p %%c
						%defstr %%c
					%endif
				%elif %%m < 3
					;symbol mode
					%if _pos = 0
						;symbol
						%strcat %%p %%p %%c
						%defstr %%c
					%else
						push_token %%p, %%m
						%defstr %%p
						%assign %%u 0
						%assign %%m -1
					%endif
				%else
					;" mode
					%strcat %%p %%p %%c
					%ifidn %%c, '"'
						push_token %%p, %%m
						%defstr %%p
						%assign %%u 0
						%assign %%m -1
					%endif
					%defstr %%c
				%endif
			%endif
			%ifidn %%c, ''
				%if %%i > %%l
					%exitrep
				%else
					%substr %%c %%s %%i, 1
					%assign %%i %%i + 1
				%endif
			%endif
		%endrep
		%ifnidn %%p, ''
			push_token %%p, %%m
		%endif
	%endmacro

	%macro print_token_list 0
		%assign %%n 0
		%rep _token_sp
			%warning token %%n: n: _token_%[%%n] t:_token_type_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

	%macro push_rpn 2
		%xdefine _rpn_%[_rpn_sp] %1
		%assign _rpn_type_%[_rpn_sp] %2
		%assign _rpn_sp _rpn_sp + 1
	%endmacro

	%macro token_to_rpn 0
		%assign _rpn_sp 0
		%assign %%o 0
		%assign %%n 0
		%rep _token_sp
			%xdefine %%t _token_%[%%n]
			%assign %%tt _token_type_%[%%n]
			%if %%tt > 3
				;operator
				%ifidn %%t, (
					%xdefine _op_%[%%o] %%t
					%assign %%o %%o + 1
				%elifidn %%t, [
					%xdefine _op_%[%%o] %%t
					%assign %%o %%o + 1
				%elifidn %%t, )
					%rep %%o
						%assign %%o %%o - 1
						%ifidn _op_%[%%o], (
							%exitrep
						%else
							push_rpn _op_%[%%o], -1
						%endif
					%endrep
				%elifidn %%t, ]
					%rep %%o
						%assign %%o %%o - 1
						%ifidn _op_%[%%o], [
							%exitrep
						%else
							push_rpn _op_%[%%o], -1
						%endif
					%endrep
					push_rpn [], -1
				%else
					;precidence
					get_sym %%t
					%if _sym = -1
						%fatal Operator %%t not defined !
					%endif
					%assign %%v _sym_value1_%[_sym] - _sym_value2_%[_sym]
					%rep %%o
						%assign %%o %%o - 1
						get_sym _op_%[%%o]
						%if %%v >= _sym_value1_%[_sym]
							push_rpn _op_%[%%o], -1
						%else
							%assign %%o %%o + 1
							%exitrep
						%endif
					%endrep
					%xdefine _op_%[%%o] %%t
					%assign %%o %%o + 1
				%endif
			%else
				;string or symbol
				push_rpn %%t, %%tt
			%endif
			%assign %%n %%n + 1
		%endrep
		%rep %%o
			%assign %%o %%o - 1
			push_rpn _op_%[%%o], -1
		%endrep
	%endmacro

	%macro print_rpn_list 0
		%assign %%n 0
		%rep _rpn_sp
			%warning rpn token %%n: t: _rpn_type_%[%%n] n: _rpn_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

	%macro inc_reg_sp 0
		%assign _reg_sp _reg_sp + 1
	%endmacro

	%macro dec_reg_sp 0
		%assign _reg_sp _reg_sp - 1
	%endmacro

	%macro set_reg 2-3 ''
		;rxx
		;type 'pppb'
		%xdefine _reg_%[%1] %2
		%xdefine _reg_t_%[%1] %3
	%endmacro

	%macro get_reg 1
		%if _reg_sp = -1
			%error Register stack underflow !
		%endif
		%if _reg_sp = _reg_total
			%error Register stack overflow !
		%endif
		%xdefine _reg _reg_%[%1]
		%xdefine _reg_t _reg_t_%[%1]
	%endmacro

	%macro pop_reg 0
		dec_reg_sp
		get_reg _reg_sp
	%endmacro

	%macro pop_value 0
		pop_reg
		compile_deref?
	%endmacro

	%macro pop_value_final 0
		pop_value
		set_reg _reg_sp, _reg
	%endmacro

	%macro set_reg_stack 0-*
		%assign _reg_sp 0
		%assign _reg_total 0
		%rep %0
			set_reg _reg_total, %1
			%assign _reg_total _reg_total + 1
			%rotate 1
		%endrep
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
			%warning param %%n: _reg, _reg_t, _reg_dt
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro compile_null 0
		%fatal Compilation code not found !
	%endmacro

	%macro compile_ref 1
		get_sym %1
		%if _sym = -1
			;not in symbol table so figure out what it is
			get_reg _reg_sp
			%ifnum %1
				;number
				set_reg _reg_sp, _reg
				%warning vp_cpy %1, _reg
				vp_cpy %1, _reg
			%elifstr %1_t
				;field/member
				set_reg _reg_sp, _reg, %1_t
				%warning vp_cpy %1, _reg
				vp_cpy %1, _reg
			%else
				;equate or register
				set_reg _reg_sp, _reg
				%ifnidn %1, _reg
					%warning vp_cpy %1, _reg
					vp_cpy %1, _reg
				%endif
			%endif
		%elif _sym_type_%[_sym] = _sym_const
			;constant
			compile_const _sym_value1_%[_sym]
		%elif _sym_type_%[_sym] = _sym_var
			;variable
			get_reg _reg_sp
			set_reg _reg_sp, _reg, _sym_aux_%[_sym]
			get_scope_offset _sym_scope_%[_sym]
			%warning vp_lea [r4 + _scope_offset + _sym_value1_%[_sym]], _reg
			vp_lea [r4 + _scope_offset + _sym_value1_%[_sym]], _reg
		%endif
	%endmacro

	%macro compile_deref 0
		get_reg _reg_sp
		%substr %%t _reg_t 1, 1
		%ifidn %%t, ''
			%error Not a reference !
		%elifidn %%t, 'l'
			%warning vp_cpy [_reg], _reg
			vp_cpy [_reg], _reg
		%elifidn %%t, 'i'
			%warning vp_cpy_i [_reg], _reg
			vp_cpy_i [_reg], _reg
		%elifidn %%t, 'B'
			%warning vp_cpy_ub [_reg], _reg
			vp_cpy_ub [_reg], _reg
		%elifidn %%t, 'L'
			%warning vp_cpy [_reg], _reg
			vp_cpy [_reg], _reg
		%elifidn %%t, 'I'
			%warning vp_cpy_ui [_reg], _reg
			vp_cpy_ui [_reg], _reg
		%elifidn %%t, 'b'
			%warning vp_cpy_b [_reg], _reg
			vp_cpy_b [_reg], _reg
		%elifidn %%t, 's'
			%warning vp_cpy_s [_reg], _reg
			vp_cpy_s [_reg], _reg
		%elifidn %%t, 'S'
			%warning vp_cpy_us [_reg], _reg
			vp_cpy_us [_reg], _reg
		%else
			%warning vp_cpy [_reg], _reg
			vp_cpy [_reg], _reg
		%endif
		%substr %%t _reg_t 2, -1
		set_reg _reg_sp, _reg, %%t
	%endmacro

	%macro compile_assign 0
		pop_reg
		%xdefine %%r _reg
		%substr %%t _reg_t 1, 1
		pop_value
		%ifidn %%t, 'L'
			%warning vp_cpy _reg, [%%r]
			vp_cpy _reg, [%%r]
		%elifidn %%t, 'l'
			%warning vp_cpy _reg, [%%r]
			vp_cpy _reg, [%%r]
		%elifidn %%t, 'i'
			%warning vp_cpy_i _reg, [%%r]
			vp_cpy_i _reg, [%%r]
		%elifidn %%t, 'I'
			%warning vp_cpy_ui _reg, [%%r]
			vp_cpy_ui _reg, [%%r]
		%elifidn %%t, 's'
			%warning vp_cpy_s _reg, [%%r]
			vp_cpy_s _reg, [%%r]
		%elifidn %%t, 'S'
			%warning vp_cpy_us _reg, [%%r]
			vp_cpy_us _reg, [%%r]
		%elifidn %%t, 'b'
			%warning vp_cpy_b _reg, [%%r]
			vp_cpy_b _reg, [%%r]
		%elifidn %%t, 'B'
			%warning vp_cpy_ub _reg, [%%r]
			vp_cpy_ub _reg, [%%r]
		%elifidn %%t, 'p'
			%warning vp_cpy _reg, [%%r]
			vp_cpy _reg, [%%r]
		%else
			%error Not a reference !
		%endif
	%endmacro

	%macro compile_field 0
		pop_reg
		%xdefine %%r _reg
		%xdefine %%t _reg_t
		pop_reg
		%warning vp_lea [_reg + %%r], _reg
		vp_lea [_reg + %%r], _reg
		set_reg _reg_sp, _reg, %%t
	%endmacro

	%macro compile_member 0
		pop_reg
		%xdefine %%r _reg
		%xdefine %%t _reg_t
		pop_reg
		compile_deref
		%warning vp_lea [_reg + %%r], _reg
		vp_lea [_reg + %%r], _reg
		set_reg _reg_sp, _reg, %%t
	%endmacro

	%macro compile_index 0
		pop_reg
		%xdefine %%r _reg
		pop_reg
		compile_deref
		%warning vp_lea [_reg + %%r], _reg
		vp_lea [_reg + %%r], _reg
	%endmacro

	%macro compile_uaddrof 0
		dec_reg_sp
		get_reg _reg_sp
		set_reg _reg_sp, _reg
	%endmacro

	%macro compile_deref? 0
		get_reg _reg_sp
		%ifnidn _reg_t, ''
			compile_deref
		%endif
	%endmacro

	%macro compile_uderef 0
		pop_value
		compile_deref
	%endmacro

	%macro compile_uminus 0
		pop_value_final
		%warning vp_mul -1, _reg
		vp_mul -1, _reg
	%endmacro

	%macro compile_unot 0
		pop_value_final
		%warning vp_xor -1, _reg
		vp_xor -1, _reg
	%endmacro

	%macro compile_ulnot 0
		pop_value_final
		%warning if _reg, ==, 0
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, ==, 0
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_land 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if %%r, !=, 0
		%warning 	if _reg, !=, 0
		%warning 		vp_cpy 1, _reg
		%warning 	else
		%warning 		vp_xor _reg, _reg
		%warning 	endif
		%warning %else
		%warning 	vp_xor _reg, _reg
		%warning %endif
		if %%r, !=, 0
			if _reg, !=, 0
				vp_cpy 1, _reg
			else
				vp_xor _reg, _reg
			endif
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_lor 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_or %%r, _reg
		vp_or %%r, _reg
	%endmacro

	%macro compile_ternary 0
		pop_value_final
		%xdefine %%r2 _reg
		pop_value_final
		%xdefine %%r1 _reg
		pop_value_final
		%warning if _reg, !=, 0
		%warning 	vp_cpy %%r1, _reg
		%warning else
		%warning 	vp_cpy %%r2, _reg
		%warning endif
		if _reg, !=, 0
			vp_cpy %%r1, _reg
		else
			vp_cpy %%r2, _reg
		endif
	%endmacro

	%macro compile_gt 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, >, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, >, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_lt 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, <, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, <, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_ge 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, >=, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, >=, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_le 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, <=, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, <=, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_eq 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, ==, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, ==, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_ne 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning if _reg, !=, %%r
		%warning 	vp_cpy 1, _reg
		%warning else
		%warning 	vp_xor _reg, _reg
		%warning endif
		if _reg, !=, %%r
			vp_cpy 1, _reg
		else
			vp_xor _reg, _reg
		endif
	%endmacro

	%macro compile_lshift 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_shl %%r, _reg
		vp_shl %%r, _reg
	%endmacro

	%macro compile_rshift 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_shr %%r, _reg
		vp_shr %%r, _reg
	%endmacro

	%macro compile_arshift 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_asr %%r, _reg
		vp_asr %%r, _reg
	%endmacro

	%macro compile_plus 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_add %%r, _reg
		vp_add %%r, _reg
	%endmacro

	%macro compile_minus 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_sub %%r, _reg
		vp_sub %%r, _reg
	%endmacro

	%macro compile_mul 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_mul %%r, _reg
		vp_mul %%r, _reg
	%endmacro

	%macro compile_div 0
		get_reg _reg_sp
		%xdefine %%r2 _reg
		pop_value_final
		%xdefine %%r1 _reg
		pop_value_final
		%warning vp_xor %%r2, %%r2
		%warning vp_div %%r1, %%r2, _reg
		vp_xor %%r2, %%r2
		vp_div %%r1, %%r2, _reg
	%endmacro

	%macro compile_rem 0
		get_reg _reg_sp
		%xdefine %%r2 _reg
		pop_value_final
		%xdefine %%r1 _reg
		pop_value_final
		%warning vp_xor %%r2, %%r2
		%warning vp_div %%r1, %%r2, _reg
		%warning vp_cpy %%r2, _reg
		vp_xor %%r2, %%r2
		vp_div %%r1, %%r2, _reg
		vp_cpy %%r2, _reg
	%endmacro

	%macro compile_and 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_and %%r, _reg
		vp_and %%r, _reg
	%endmacro

	%macro compile_xor 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_xor %%r, _reg
		vp_xor %%r, _reg
	%endmacro

	%macro compile_or 0
		pop_value_final
		%xdefine %%r _reg
		pop_value_final
		%warning vp_or %%r, _reg
		vp_or %%r, _reg
	%endmacro

	%macro compile_operator 1
		get_sym %1
		%if _sym = -1
			%error Operator %1 not defined !
		%elif _sym_type_%[_sym] != _sym_op
			%error %1 not an operator !
		%endif
		_sym_aux_%[_sym]
	%endmacro

	%macro compile_string 1
		get_reg _reg_sp
		set_reg _reg_sp, _reg
		%warning fn_string %1, _reg
		fn_string %1, _reg
	%endmacro

	%macro compile_bind 1
		get_reg _reg_sp
		set_reg _reg_sp, _reg
		%warning fn_bind %1, _reg
		fn_bind %1, _reg
	%endmacro

	%macro compile_label 1
		get_reg _reg_sp
		set_reg _reg_sp, _reg
		%warning vp_rel %1, _reg
		vp_rel %1, _reg
	%endmacro

	%macro compile_const 1
		get_reg _reg_sp
		set_reg _reg_sp, _reg
		%warning vp_cpy %1, _reg
		vp_cpy %1, _reg
	%endmacro

	%macro compile_rpn_list 0
		%assign %%n 0
		%rep _rpn_sp
			%xdefine %%t _rpn_%[%%n]
			%assign %%tt _rpn_type_%[%%n]
			%if %%tt = -1
				;operator
				compile_operator %%t
			%elif %%tt = 0
				;...
				compile_ref %%t
			%elif %%tt = 1
				;@...
				compile_bind %%t
			%elif %%tt = 2
				;$...
				compile_label %%t
			%elif %%tt = 3
				;"..."
				compile_string %%t
			%else
				%error Unknown token type %%t !
			%endif
			inc_reg_sp
			;print_reg_stack
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

	%macro eval 2
		%warning --------------------------------------
		set_src %1
		set_reg_stack %2
		%if _src_total != _reg_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		fill_reg_stack
		%assign %%n 0
		%rep _src_total
			set_token_list _src_%[%%n]
			token_to_rpn
			compile_rpn_list
			pop_value_final
			inc_reg_sp
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro retire 2
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		set_reg_stack
		%assign %%n 0
		%rep _src_total
			add_reg_stack _src_%[%%n]
			%assign %%n %%n + 1
		%endrep
		fill_reg_stack
		%assign _reg_sp _src_total
		%rep _dst_total
			%assign %%r _reg_sp - 1
			set_token_list _dst_%[%%r]
			token_to_rpn
			compile_rpn_list
			compile_assign
		%endrep
	%endmacro

	%macro assign 2
		%warning --------------------------------------
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		set_reg_stack
		fill_reg_stack
		%assign %%n 0
		%rep _src_total
			set_token_list _src_%[%%n]
			token_to_rpn
			compile_rpn_list
			%assign %%n %%n + 1
		%endrep
		%assign _reg_sp _src_total
		%rep _dst_total
			%assign %%r _reg_sp - 1
			set_token_list _dst_%[%%r]
			token_to_rpn
			compile_rpn_list
			compile_assign
		%endrep
	%endmacro

%endif
