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
		%else
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
		%endif
	%endmacro

%endif
