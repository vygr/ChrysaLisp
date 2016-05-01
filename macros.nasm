;;;;;;;;;;;;;;
; symbol table
;;;;;;;;;;;;;;

	%assign _sym_total 0
	_sym_op		equ 0
	_sym_const	equ 1
	_sym_var	equ 2

	%macro def_sym 3
		;%1 name
		;%2 type
		;%3 value
		%assign %%n 0
		%rep _sym_total
			%ifidn _sym_name_%[%%n], %1
				%exitrep
			%endif
			%assign %%n %%n + 1
		%endrep
		%if %%n != _sym_total
			%error Symbol %1 redefined !
		%else
			%xdefine _sym_name_%[%%n] %1
			%assign _sym_type_%[%%n] %2
			%assign _sym_value_%[%%n] %3
			%assign _sym_total _sym_total + 1
		%endif
	%endmacro

	%macro redef_sym 3
		;%1 name
		;%2 type
		;%3 value
		%assign %%n 0
		%rep _sym_total
			%ifidn _sym_name_%[%%n], %1
				%exitrep
			%endif
			%assign %%n %%n + 1
		%endrep
		%if %%n = _sym_total
			%error Symbol %1 not found !
		%else
			%xdefine _sym_name_%[%%n] %1
			%assign _sym_type_%[%%n] %2
			%assign _sym_value_%[%%n] %3
		%endif
	%endmacro

	%macro get_sym 1
		;%1 name
		%assign %%n 0
		%assign _sym -1
		%rep _sym_total
			%ifidn _sym_name_%[%%n], %1
				%assign _sym %%n
				%exitrep
			%endif
			%assign %%n %%n + 1
		%endrep
	%endmacro

	%macro print_sym 0
		%assign %%n 0
		%rep _sym_total
			%warning _sym_name_%[%%n], _sym_type_%[%%n], _sym_value_%[%%n]
			%assign %%n %%n + 1
		%endrep
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

;;;;;;;;;;;;;;
; token parser
;;;;;;;;;;;;;;

	%macro push_token 1
		%deftok %%t %1
		%xdefine _token_%[_token_total] %%t
		%assign _token_total _token_total + 1
	%endmacro

	%macro set_token_list 1
		%defstr %%s %1
		%strlen %%l %%s
		%assign _token_total 0
		%defstr %%p
		%assign %%m -1
		%assign %%u 1
		%assign %%i 1
		%rep %%l
			%substr %%ss %%s %%i, 1
			sub_string %%ss, '"-+*/%&^|[]() '
			%if %%m = -1
				;op mode
				%ifnidn %%ss, ' '
					%if _pos = 0
						;none
						%strcat %%p %%p %%ss
						%assign %%m _pos
					%elif _pos = 1
						;"
						%strcat %%p %%p %%ss
						%assign %%m _pos
					%elif _pos = 2
						;-
						%if %%u = 1
							push_token '_'
						%else
							push_token %%ss
							%assign %%u 1
						%endif
					%else
						;+*/%&^|[]()
						push_token %%ss
						%assign %%u 1
					%endif
				%endif
			%elif %%m = 0
				;symbol mode
				%if _pos = 0
					%strcat %%p %%p %%ss
				%else
					push_token %%p
					%defstr %%p
					%assign %%u 0
					%assign %%m -1

					;op mode
					%ifnidn %%ss, ' '
						%if _pos = 0
							;none
							%strcat %%p %%p %%ss
							%assign %%m _pos
						%elif _pos = 1
							;"
							%strcat %%p %%p %%ss
							%assign %%m _pos
						%elif _pos = 2
							;-
							%if %%u = 1
								push_token '_'
							%else
								push_token %%ss
								%assign %%u 1
							%endif
						%else
							;+*/%&^|[]()
							push_token %%ss
							%assign %%u 1
						%endif
					%endif
				%endif
			%else
				;string mode
				%strcat %%p %%p %%ss
				%if _pos = 1
					push_token %%p
					%defstr %%p
					%assign %%u 0
					%assign %%m -1
				%endif
			%endif
			%assign %%i %%i + 1
		%endrep
		%ifnidn %%p, ''
			push_token %%p
		%endif
	%endmacro

	%macro print_token_list 0
		%assign %%n 0
		%rep _token_total
			%warning token %%n: _token_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;;;;
; reverse polish
;;;;;;;;;;;;;;;;

	%macro push_rpn 1
		%xdefine _rpn_%[_rpn_total] %1
		%assign _rpn_total _rpn_total + 1
	%endmacro

	%macro token_to_rpn 0
		%assign _rpn_total 0
		%assign %%o 0
		%assign %%n 0
		%rep _token_total
			%ifnstr _token_%[%%n]
				;not string
				get_sym _token_%[%%n]
				%if _sym = -1
					%fatal Symbol _token_%[%%n] not found !
				%endif
				%if _sym_type_%[_sym] = _sym_op
					;operator
					%ifidn _token_%[%%n], (
						%xdefine _op_%[%%o] _token_%[%%n]
						%assign %%o %%o + 1
					%elifidn _token_%[%%n], )
						%rep %%o
							%assign %%o %%o - 1
							%ifidn _op_%[%%o], (
								%exitrep
							%else
								push_rpn _op_%[%%o]
							%endif
						%endrep
					%else
						;precidence
						%assign %%t _sym_value_%[_sym]
						%if %%t = 0
							;unary -
							%assign %%t -1
						%endif
						%rep %%o
							%assign %%o %%o - 1
							get_sym _op_%[%%o]
							%if %%t < _sym_value_%[_sym]
								%assign %%o %%o + 1
								%exitrep
							%else
								push_rpn _op_%[%%o]
							%endif
						%endrep
						%xdefine _op_%[%%o] _token_%[%%n]
						%assign %%o %%o + 1
					%endif
				%else
					;symbol
					push_rpn _token_%[%%n]
				%endif
			%else
				;string
				push_rpn _token_%[%%n]
			%endif
			%assign %%n %%n + 1
		%endrep
		%rep %%o
			%assign %%o %%o - 1
			push_rpn _op_%[%%o]
		%endrep
	%endmacro

	%macro print_rpn_list 0
		%assign %%n 0
		%rep _rpn_total
			%warning rpn token %%n: _rpn_%[%%n]
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;;
; compilation
;;;;;;;;;;;;;

	%macro inc_reg_sp 0
		%assign _reg_sp _reg_sp + 1
		%if _reg_sp = _reg_total
			%error Register stack overflow !
		%endif
	%endmacro

	%macro dec_reg_sp 0
		%assign _reg_sp _reg_sp - 1
		%if _reg_sp = -1
			%error Register stack underflow !
		%endif
	%endmacro

	%macro set_reg 2
		%xdefine _reg_%[%1] %2
	%endmacro

	%macro get_reg 1
		%xdefine _reg _reg_%[%1]
	%endmacro

	%macro pop_reg 0
		dec_reg_sp
		get_reg _reg_sp
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

	%macro compile_rpn_list 0
		%assign %%n 0
		%rep _rpn_total
			%ifnstr _rpn_%[%%n]
				;not string
				get_sym _rpn_%[%%n]
				%if _sym_type_%[_sym] = _sym_op
					;operator
					%ifidn _rpn_%[%%n], _
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_mul -1, %%r0
					%elifidn _rpn_%[%%n], +
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_add %%r1, %%r0
					%elifidn _rpn_%[%%n], -
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_sub %%r1, %%r0
					%elifidn _rpn_%[%%n], *
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_mul %%r1, %%r0
					%elifidn _rpn_%[%%n], &
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_and %%r1, %%r0
					%elifidn _rpn_%[%%n], ^
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_xor %%r1, %%r0
					%elifidn _rpn_%[%%n], |
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_or %%r1, %%r0
					%elifidn _rpn_%[%%n], /
						get_reg _reg_sp
						%xdefine %%r2 _reg
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_xor %%r2, %%r2
						%warning vp_div %%r1, %%r2, %%r0
					%elifidn _rpn_%[%%n], %
						get_reg _reg_sp
						%xdefine %%r2 _reg
						pop_reg
						%xdefine %%r1 _reg
						pop_reg
						%xdefine %%r0 _reg
						%warning vp_xor %%r2, %%r2
						%warning vp_div %%r1, %%r2, %%r0
						%warning vp_cpy %%r2, %%r0
					%endif
				%elif _sym_type_%[_sym] = _sym_const
					;constant
					get_reg _reg_sp
					%warning vp_cpy _sym_value_%[_sym], _reg
				%elif _sym_type_%[_sym] = _sym_var
					;variable
					get_reg _reg_sp
					%warning vp_cpy [r4 + _sym_value_%[_sym]], _reg
				%endif
			%else
				;string
				get_reg _reg_sp
				%warning fn_string _rpn_%[%%n], _reg
			%endif
			inc_reg_sp
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;;
; assignment
;;;;;;;;;;;;

	%macro assign 2
		%warning --------------------------------------
		%warning {%1}, {%2}
		%warning --------------------------------------
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
			add_reg_stack _dst_%[%%n]
			%assign %%n %%n + 1
		%endrep
		fill_reg_stack
		%assign %%n 0
		%rep _src_total
			set_token_list _src_%[%%n]
			token_to_rpn
			compile_rpn_list
			%assign %%n %%n + 1
		%endrep
	%endmacro

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	;define the operators
	;value is precidence
	def_sym	_, _sym_op, 0
	def_sym	*, _sym_op, 1
	def_sym	/, _sym_op, 1
	def_sym	%, _sym_op, 1
	def_sym	+, _sym_op, 2
	def_sym	-, _sym_op, 2
	def_sym	&, _sym_op, 3
	def_sym	^, _sym_op, 4
	def_sym	|, _sym_op, 5
	def_sym	(, _sym_op, 6
	def_sym	), _sym_op, 6

	;define constants
	def_sym	a, _sym_const, 0
	def_sym	b, _sym_const, 1
	def_sym	c, _sym_const, 2
	def_sym	d, _sym_const, 3
	def_sym	e, _sym_const, 4
	def_sym	f, _sym_const, 5

	;define variables
	def_sym	xxx, _sym_var, 16
	def_sym	yyy, _sym_var, 32
	def_sym	zzz, _sym_var, 48

	print_sym

	assign {a + b * xxx}, {r0}
