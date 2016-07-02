%ifndef CODE_1234
%define CODE_1234

%include 'inc/vp.inc'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; things I wish sombody would add to NASM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro search_string 2
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

	%macro replace_string 4
		;%1 = param to insert
		;%2 = param to remove
		;%3 = param to splice
		;%4 = position to insert
		%strlen %%l %2
		%substr %%s1 %3 1, %4 - 1
		%substr %%s2 %3 %4 + %%l, -1
		%strcat _str %%s1 %1 %%s2
	%endmacro

;;;;;;;;;;;;;;;;;;;;
; structured codeing
;;;;;;;;;;;;;;;;;;;;

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
		vp_jmpifnot r0, !=, 0, %$_loc_%$_cnt
	%endmacro

	%macro case 3
		default
		vp_jmpifnot %1, %2, %3, %$_loc_%$_cnt
	%endmacro

	%macro casenot 1
		default
		eval {%1}, {r0}
		vp_jmpif r0, !=, 0, %$_loc_%$_cnt
	%endmacro

	%macro casenot 3
		default
		vp_jmpif %1, %2, %3, %$_loc_%$_cnt
	%endmacro

	%macro break 0
		vp_jmp %$_end
	%endmacro

	%macro breakif 3
		vp_jmpif %1, %2, %3, %$_end
	%endmacro

	%macro breakif 1
		eval {%1}, {r0}
		breakif r0, !=, 0
	%endmacro

	%macro breakifnot 3
		vp_jmpifnot %1, %2, %3, %$_end
	%endmacro

	%macro breakifnot 1
		eval {%1}, {r0}
		breakifnot r0, !=, 0
	%endmacro

	%macro continue 0
		vp_jmp %$_begin
	%endmacro

	%macro continueif 3
		vp_jmpif %1, %2, %3, %$_begin
	%endmacro

	%macro continueif 1
		eval {%1}, {r0}
		continueif r0, !=, 0
	%endmacro

	%macro continueifnot 3
		vp_jmpifnot %1, %2, %3, %$_begin
	%endmacro

	%macro continueifnot 1
		eval {%1}, {r0}
		continueifnot r0, !=, 0
	%endmacro

	%macro gotoif 2
		eval {%1}, {r0}
		vp_jmpif r0, !=, 0, %2
	%endmacro

	%macro gotoifnot 2
		eval {%1}, {r0}
		vp_jmpifnot r0, !=, 0, %2
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

;;;;;;;
; enums
;;;;;;;

	%macro def_enum 0-1 0
		;inputs
		;%1 = start offset
		%assign _enum_ %1
	%endmacro

	%macro def_enum_end 0
		%undef _enum_
	%endmacro

	%macro enum 1
		%1 equ _enum_
		%assign _enum_ _enum_ + 1
	%endmacro

;;;;;;
; bits
;;;;;;

	%macro def_bit 0-1 0
		;inputs
		;%1 = start bit
		%assign _bit_ %1
	%endmacro

	%macro def_bit_end 0
		%undef _bit_
	%endmacro

	%macro bit 1
		%1 equ 1 << _bit_
		%assign _bit_ _bit_ + 1
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

	%macro offset 1
		;inputs
		;%1 = var name
		%ifdef _struct_
			%1 equ _offset_
		%else
			%error Atempt to define offset into local stack
		%endif
	%endmacro

	%macro ptr 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'p'
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
		struct %1, int, 'i'
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

	%macro pptr 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pp'
	%endmacro

	%macro pubyte 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pB'
	%endmacro

	%macro pushort 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pS'
	%endmacro

	%macro puint 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pI'
	%endmacro

	%macro pulong 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pL'
	%endmacro

	%macro pbyte 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pb'
	%endmacro

	%macro pshort 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'ps'
	%endmacro

	%macro pint 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pi'
	%endmacro

	%macro plong 1
		;inputs
		;%1 = var name
		local_align ptr
		struct %1, ptr, 'pl'
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
			%ifdef debug_emit
				%warning vp_sub _var_sp, r4
			%endif
			vp_sub _var_sp, r4
			%assign _var_sp 0
		%endif
	%endmacro

	%macro pop_scope 0
		%assign _scope_sp _scope_sp - 1
		%assign _sym_sp _scope_sym_%[_scope_sp]
		%assign _var_sp _scope_var_%[_scope_sp]
		%if _var_sp != 0
			%ifdef debug_emit
				%warning vp_add _var_sp, r4
			%endif
			vp_add _var_sp, r4
			%assign _var_sp 0
		%endif
		%assign %%s _scope_sp - 1
		%if %%s != -1
			%assign _sym_sp _scope_sym_%[%%s]
		%endif
	%endmacro

	%macro unwind 0
		get_scope_offset 0
		%if _scope_offset != 0
			%ifdef debug_emit
				%warning vp_add _scope_offset, r4
			%endif
			vp_add _scope_offset, r4
		%endif
	%endmacro

	%macro return 0
		unwind
		%ifdef debug_emit
			%warning vp_ret
		%endif
		vp_ret
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
			%defstr _src_%[_src_total] %1
			%assign _src_total _src_total + 1
			%rotate 1
		%endrep
	%endmacro

	%macro set_dst 0-*
		;%1... = paramaters
		%assign _dst_total 0
		%rep %0
			%defstr _dst_%[_dst_total] %1
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
		%xdefine %%s _src_%2
		%xdefine %%d _dst_%2
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
			%ifnidn _dst_%[%%j], '_'
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

	%macro find_later_src 1
		;%1 = index of dst
		%assign _idx -1
		%assign %%i _src_total - 1
		%rep %%i - %1
			%deftok %%s _src_%[%%i]
			%ifnstr %%s
				%ifnnum %%s
					search_string _dst_%1, _src_%[%%i]
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
		%assign %%i 0
		%rep 1000
			%rep _dst_total - %%i
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
		%endrep
		%if %%i != _dst_total
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
			%deftok %%s _src_%[%%i]
			%deftok %%d _dst_%[%%i]
			%ifstr %%s
				;string
				fn_string %%s, %%d
			%else
				%substr %%ss _src_%[%%i] 1, 1
				%ifidn %%ss, '@'
					;bind function
					%substr %%ss _src_%[%%i] 2, -1
					%deftok %%p %%ss
					fn_bind %%p, %%d
				%elifidn %%ss, '&'
					;address of
					%substr %%ss _src_%[%%i] 2, -1
					%deftok %%p %%ss
					vp_lea %%p, %%d
				%elifidn %%ss, '$'
					;label address
					%substr %%ss _src_%[%%i] 2, -1
					%deftok %%p %%ss
					vp_rel %%p, %%d
				%else
					;just a copy
					vp_cpy %%s, %%d
				%endif
			%endif
			%assign %%i %%i + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;;
; token parser
;;;;;;;;;;;;;;

	%macro push_token 2
		%xdefine _token_%[_token_sp] %1
		%assign _token_type_%[_token_sp] %2
		%assign _token_sp _token_sp + 1
	%endmacro

	%macro set_token_list 1
		%strlen %%l %1
		%assign _token_sp 0
		%defstr %%p
		%defstr %%c
		%assign %%m -1
		%assign %%u 1
		%assign %%i 1
		%rep %%l * 2
			%ifnidn %%c, ''
				search_string %%c, '@$"-*~!+.?^</%&|=> )(]['
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
					%if _pos > 18 || _pos < 12
						;space, )([] or not </%&|=>
						%if %%u = 1
							;unary operator
							search_string %%p, '-*&~!'
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
					%substr %%c %1 %%i, 1
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
			%deftok %%t _token_%[%%n]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; instruction output buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro add_inst 1-*
		%assign _i_0_%[_i_total] %0
		%assign %%i 1
		%rep %0
			%defstr _i_%[%%i]_%[_i_total] %1
			%assign %%i %%i + 1
			%rotate 1
		%endrep
		%rep 4 - %0
			%defstr _i_%[%%i]_%[_i_total]
			%assign %%i %%i + 1
		%endrep
		%assign _i_total _i_total + 1
	%endmacro

	%macro sub_inst 1
		%assign _i_0_%1 0
		%defstr _i_1_%1
		%defstr _i_2_%1
		%defstr _i_3_%1
		%defstr _i_4_%1
	%endmacro

	%macro opt_inst_list 0
		;reference pass
		%assign %%i 0
		%rep _i_total
			%ifidn _i_1_%[%%i], 'op_ref'
				%assign %%p %%i + 1
				%rep _i_total - %%p
					%ifidn _i_1_%[%%p], 'op_deref'
						%ifidn _i_3_%[%%p], _i_3_%[%%i]
							;op_ref => op_deref
							%assign _i_0_%[%%i] _i_0_%[%%p]
							%xdefine _i_1_%[%%i] _i_1_%[%%p]
							%xdefine _i_4_%[%%i] _i_4_%[%%p]
							sub_inst %%p
							%exitrep
						%endif
					%elifidn _i_1_%[%%p], 'op_assign'
						%ifidn _i_3_%[%%p], _i_3_%[%%i]
							;op_ref => op_assign
							%xdefine _i_3_%[%%p] _i_2_%[%%i]
							sub_inst %%i
							%exitrep
						%endif
					%endif
					search_string _i_3_%[%%i], _i_2_%[%%p]
					%if _pos != 0
						;use of ref as source
						%exitrep
					%endif
					search_string _i_3_%[%%i], _i_3_%[%%p]
					%if _pos != 0
						;use of ref as destination
						%exitrep
					%endif
					%assign %%p %%p + 1
				%endrep
			%endif
			%assign %%i %%i + 1
		%endrep

		;constants pass
		%assign %%i 0
		%rep _i_total
			%ifidn _i_1_%[%%i], 'op_const'
				%assign %%p %%i + 1
				%rep _i_total - %%p
					search_string _i_3_%[%%i], _i_2_%[%%p]
					%if _pos != 0
						;only if not an assign
						%ifnidn _i_1_%[%%p], 'op_assign'
							;op_const => op_imm
							replace_string _i_2_%[%%i], _i_3_%[%%i], _i_2_%[%%p], _pos
							%xdefine _i_2_%[%%p] _str
							sub_inst %%i
						%endif
						%exitrep
					%else
						search_string _i_3_%[%%i], _i_3_%[%%p]
						%if _pos != 0
							;only if destination is not register
							is_reg _i_3_%[%%p]
							%if _f < 0
								;op_const => op_imm
								replace_string _i_2_%[%%i], _i_3_%[%%i], _i_3_%[%%p], _pos
								%xdefine _i_3_%[%%p] _str
								sub_inst %%i
							%endif
							%exitrep
						%endif
					%endif
					%assign %%p %%p + 1
				%endrep
			%endif
			%assign %%i %%i + 1
		%endrep
	%endmacro

	%macro emit_inst 1
		;single point to emit the instruction stream
		%ifdef debug_emit
			%warning %1
		%endif
		%1
	%endmacro

	%macro emit_info 1
		;single point to emit the instruction stream info
		%ifdef debug_emit
			%warning %1
		%endif
	%endmacro

	%macro emit_copy 3
		%ifidn %3, 'i'
			%xdefine %%t _i
		%elifidn %3, 'I'
			%xdefine %%t _ui
		%elifidn %3, 's'
			%xdefine %%t _s
		%elifidn %3, 'S'
			%xdefine %%t _us
		%elifidn %3, 'b'
			%xdefine %%t _b
		%elifidn %3, 'B'
			%xdefine %%t _ub
		%else
			%xdefine %%t
		%endif
		emit_inst {vp_cpy%[%%t] %1, %2}
	%endmacro

	%macro emit_inst_list 1
		;%1 info
		%if _i_total != 0
			emit_info {%1}
			%assign %%i 0
			%rep _i_total
				%assign %%n _i_0_%[%%i]
				%deftok %%1 _i_1_%[%%i]
				%deftok %%2 _i_2_%[%%i]
				%deftok %%3 _i_3_%[%%i]
				%deftok %%4 _i_4_%[%%i]
				%if %%n = 1
					emit_inst {%%1}
				%elif %%n = 2
					emit_inst {%%1 %%2}
				%elif %%n = 3
					%ifidn %%1, op_ref
						emit_inst {vp_lea [%%2], %%3}
					%elifidn %%1, op_const
						emit_inst {vp_cpy %%2, %%3}
					%else
						emit_inst {%%1 %%2, %%3}
					%endif
				%elif %%n = 4
					%ifidn %%1, op_deref
						emit_copy [%%2], %%3, %%4
					%elifidn %%1, op_assign
						emit_copy %%2, [%%3], %%4
					%else
						emit_inst {%%1 %%2, %%3, %%4}
					%endif
				%endif
				%assign %%i %%i + 1
			%endrep
		%endif
	%endmacro

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

	%macro reset_reg_stack 0-1 0
		;%1 = reserved slots
		%assign _s 0
		%rep %1
			%defstr _%[_s]_t
			%assign _s _s + 1
		%endrep
		%assign _s %1
		%assign _r_total 0
		%assign _i_total 0
	%endmacro

	%macro res_reg_def 1
		;%1 = 'rX'
		%deftok _%[_r_total] %1
		%defstr _%[_r_total]_t
		%assign _r_total _r_total + 1
	%endmacro

	%macro add_reg_def 1
		;%1 = rX
		%assign %%n 0
		%rep _r_total
			%ifidn _%[%%n], %1
				%exitrep
			%else
				%assign %%n %%n + 1
			%endif
		%endrep
		%if %%n = _r_total
			%xdefine _%[_r_total] %1
			%assign _r_total _r_total + 1
		%endif
	%endmacro

	%macro fill_reg_def 0
		add_reg_def r0
		add_reg_def r1
		add_reg_def r2
		add_reg_def r3
		add_reg_def r5
		add_reg_def r6
		add_reg_def r7
		add_reg_def r8
		add_reg_def r9
		add_reg_def r10
		add_reg_def r11
		add_reg_def r12
		add_reg_def r13
		add_reg_def r14
		add_reg_def r15
	%endmacro

	%macro undef_reg_def 0
		%rep _r_total
			%assign _r_total _r_total - 1
			%undef _%[_r_total]
			%undef _%[_r_total]_t
		%endrep
	%endmacro

	%macro print_reg_stack 0
		%assign %%n 0
		%rep _s
			%warning param %%n: _%[%%n], _%[%%n]_t
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro is_reg 1
		%assign _f 16 - 1
		%deftok %%t %1
		%rep 16
			%ifidn _%[_f], %%t
				%exitrep
			%else
				%assign _f _f - 1
			%endif
		%endrep
	%endmacro

	%macro pop_value 0
		%assign _s _s - 1
		compile_deref?
		%defstr _%[_s]_t
	%endmacro

	%macro compile_null 0
		%fatal Compilation code not found !
	%endmacro

	%macro compile_ref 1
		get_sym %1
		%if _sym = -1
			;not in symbol table so figure out what it is
			%ifnum %1
				;number
				%defstr _%[_s]_t
				add_inst op_const, %1, _%[_s]
			%elifstr %1_t
				;field/member
				%xdefine _%[_s]_t %1_t
				add_inst op_const, %1, _%[_s]
			%elifidn %1, RXX
				;reserved slot
				%defstr _%[_s]_t
			%else
				;equate
				%defstr _%[_s]_t
				add_inst op_const, %1, _%[_s]
			%endif
		%elif _sym_type_%[_sym] = _sym_const
			;constant
			compile_const _sym_value1_%[_sym]
		%elif _sym_type_%[_sym] = _sym_var
			;variable
			%xdefine _%[_s]_t _sym_aux_%[_sym]
			get_scope_offset _sym_scope_%[_sym]
			add_inst op_ref, r4 + _scope_offset + _sym_value1_%[_sym], _%[_s]
		%endif
	%endmacro

	%macro compile_deref 0
		%ifidn _%[_s]_t, ''
			%error No type info !
		%endif
		%substr %%t _%[_s]_t 1, 1
		add_inst op_deref, _%[_s], _%[_s], %%t
		%substr %%t _%[_s]_t 2, -1
		%xdefine _%[_s]_t %%t
	%endmacro

	%macro compile_deref? 0
		%ifnidn _%[_s]_t, ''
			compile_deref
		%endif
	%endmacro

	%macro compile_assign 0
		%assign _s _s - 1
		%assign %%s _s
		%ifidn _%[_s]_t, ''
			%error No type info !
		%endif
		%substr %%t _%[_s]_t 1, 1
		pop_value
		add_inst op_assign, _%[_s], _%[%%s], %%t
	%endmacro

	%macro compile_field 0
		%assign %%s _s - 1
		%assign _s _s - 2
		add_inst op_ref, _%[_s] + _%[%%s], _%[_s]
		%xdefine _%[_s]_t _%[%%s]_t
	%endmacro

	%macro compile_member 0
		%assign %%s _s - 1
		%assign _s _s - 2
		compile_deref
		add_inst op_ref, _%[_s] + _%[%%s], _%[_s]
		%xdefine _%[_s]_t _%[%%s]_t
	%endmacro

	%macro compile_index 0
		pop_value
		%assign %%s _s
		%assign _s _s - 1
		compile_deref
		add_inst op_ref, _%[_s] + _%[%%s], _%[_s]
	%endmacro

	%macro compile_uaddrof 0
		%assign _s _s - 1
		%defstr _%[_s]_t
	%endmacro

	%macro compile_uderef 0
		%assign _s _s - 1
		compile_deref
		add_inst op_ref, _%[_s], _%[_s]
	%endmacro

	%macro compile_uminus 0
		pop_value
		add_inst vp_mul, -1, _%[_s]
	%endmacro

	%macro compile_unot 0
		pop_value
		add_inst vp_xor, -1, _%[_s]
	%endmacro

	%macro compile_ulnot 0
		pop_value
		add_inst if, _%[_s], ==, 0
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_land 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[%%s], !=, 0
		add_inst  if, _%[_s], !=, 0
		add_inst   vp_cpy, 1, _%[_s]
		add_inst  else
		add_inst   vp_xor, _%[_s], _%[_s]
		add_inst  endif
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_lor 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_or, _%[%%s], _%[_s]
	%endmacro

	%macro compile_ternary 0
		pop_value
		%assign %%s2 _s
		pop_value
		%assign %%s1 _s
		pop_value
		add_inst if, _%[_s], !=, 0
		add_inst  vp_cpy, _%[%%s1], _%[_s]
		add_inst else
		add_inst  vp_cpy, _%[%%s2], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_gt 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], >, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_lt 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], <, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_ge 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], >=, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_le 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], <=, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_eq 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], ==, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_ne 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst if, _%[_s], !=, _%[%%s]
		add_inst  vp_cpy, 1, _%[_s]
		add_inst else
		add_inst  vp_xor, _%[_s], _%[_s]
		add_inst endif
	%endmacro

	%macro compile_lshift 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_shl, _%[%%s], _%[_s]
	%endmacro

	%macro compile_rshift 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_shr, _%[%%s], _%[_s]
	%endmacro

	%macro compile_arshift 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_asr, _%[%%s], _%[_s]
	%endmacro

	%macro compile_plus 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_add, _%[%%s], _%[_s]
	%endmacro

	%macro compile_minus 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_sub, _%[%%s], _%[_s]
	%endmacro

	%macro compile_mul 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_mul, _%[%%s], _%[_s]
	%endmacro

	%macro compile_div 0
		%assign %%s2 _s
		pop_value
		%assign %%s1 _s
		pop_value
		add_inst vp_ext, _%[_s], _%[%%s2]
		add_inst vp_div, _%[%%s1], _%[%%s2], _%[_s]
	%endmacro

	%macro compile_rem 0
		%assign %%s2 _s
		pop_value
		%assign %%s1 _s
		pop_value
		add_inst vp_ext, _%[_s], _%[%%s2]
		add_inst vp_div, _%[%%s1], _%[%%s2], _%[_s]
		add_inst vp_cpy, _%[%%s2], _%[_s]
	%endmacro

	%macro compile_divu 0
		%assign %%s2 _s
		pop_value
		%assign %%s1 _s
		pop_value
		add_inst vp_xor, _%[%%s2], _%[%%s2]
		add_inst vp_div_u, _%[%%s1], _%[%%s2], _%[_s]
	%endmacro

	%macro compile_remu 0
		%assign %%s2 _s
		pop_value
		%assign %%s1 _s
		pop_value
		add_inst vp_xor, _%[%%s2], _%[%%s2]
		add_inst vp_div_u, _%[%%s1], _%[%%s2], _%[_s]
		add_inst vp_cpy, _%[%%s2], _%[_s]
	%endmacro

	%macro compile_and 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_and, _%[%%s], _%[_s]
	%endmacro

	%macro compile_xor 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_xor, _%[%%s], _%[_s]
	%endmacro

	%macro compile_or 0
		pop_value
		%assign %%s _s
		pop_value
		add_inst vp_or, _%[%%s], _%[_s]
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
		%defstr _%[_s]_t
		add_inst fn_string, %1, _%[_s]
	%endmacro

	%macro compile_bind 1
		%defstr _%[_s]_t
		add_inst fn_bind, %1, _%[_s]
	%endmacro

	%macro compile_label 1
		%defstr _%[_s]_t
		add_inst vp_rel, %1, _%[_s]
	%endmacro

	%macro compile_const 1
		%defstr _%[_s]_t
		add_inst op_const, %1, _%[_s]
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
			%assign _s _s + 1
			;print_reg_stack
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

	%macro eval 2
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		%if _dst_total != 0
			reset_reg_stack
			%rep _src_total
				set_token_list _src_%[_s]
				token_to_rpn
				compile_rpn_list
				pop_value
				%assign _s _s + 1
			%endrep
			opt_inst_list
			%assign %%n 0
			%rep _dst_total
				res_reg_def _dst_%[%%n]
				%assign %%n %%n + 1
			%endrep
			fill_reg_def
			emit_inst_list {eval: %1 => %2}
			undef_reg_def
		%endif
	%endmacro

	%macro retire 2
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		%if _dst_total != 0
			reset_reg_stack _src_total
			%rep _dst_total
				%assign %%s _s - 1
				set_token_list _dst_%[%%s]
				token_to_rpn
				compile_rpn_list
				compile_assign
			%endrep
			opt_inst_list
			%assign %%n 0
			%rep _src_total
				res_reg_def _src_%[%%n]
				%assign %%n %%n + 1
			%endrep
			fill_reg_def
			emit_inst_list {retire: %1 => %2}
			undef_reg_def
		%endif
		%ifdef debug_lines
			fn_debug_str 'done'
		%endif
	%endmacro

	%macro assign 2
		set_src %1
		set_dst %2
		%if _dst_total != _src_total
			%fatal Mismatching number of src/dst paramaters !
		%endif
		map_remove_ignored
		%if _dst_total != 0
			reset_reg_stack
			%rep _src_total
				set_token_list _src_%[_s]
				token_to_rpn
				compile_rpn_list
			%endrep
			%rep _dst_total
				%assign %%s _s - 1
				set_token_list _dst_%[%%s]
				token_to_rpn
				compile_rpn_list
				compile_assign
			%endrep
			opt_inst_list
			fill_reg_def
			emit_inst_list {assign: %1 => %2}
			undef_reg_def
		%endif
		%ifdef debug_lines
			fn_debug_str 'done'
		%endif
	%endmacro

%endif
