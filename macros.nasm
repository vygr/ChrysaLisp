%include 'inc/code.inc'

%macro set_reg 2
	%xdefine _reg_%[%1] %2
%endmacro

%macro get_reg 1
	%xdefine _reg _reg_%[%1]
%endmacro

%macro push_reg 0
	%assign _reg_sp _reg_sp + 1
	%if _reg_sp = _reg_total
		%fatal Register stack overflow !
	%endif
%endmacro

%macro pop_reg 0
	%assign _reg_sp _reg_sp - 1
	%if _reg_sp = -1
		%fatal Register stack underflow !
	%endif
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

%macro set_reg_stack 0-*
	%assign _reg_sp 0
	%assign _reg_total 0
	%rep %0
		set_reg _reg_total, %1
		%assign _reg_total _reg_total + 1
		%rotate 1
	%endrep
	fill_reg_stack
%endmacro

%macro get_reg_stack 0
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
		%fatal Unknown operator %1
	%endif
%endmacro

%macro push_token 2
	%xdefine _token_%[_token_total] %1
	%assign _token_type_%[_token_total] %2
	%assign _token_total _token_total + 1
%endmacro

%macro set_token_list 1
	%defstr %%s %1
	%strlen %%l %%s
	%assign _token_total 0
	%xdefine %%p
	%assign %%m -1
	%assign %%i 1
	%rep %%l
		%substr %%ss %%s %%i, 1
		sub_string %%ss, '[:@$] ()+-*/%&^|'
		%if %%m = -1
			%if _pos = 0
				%deftok %%t %%ss
				%xdefine %%p %[%%p]%[%%t]
			%elif _pos = 6
				%ifnempty %%p
					push_token %%p, %%m
					%xdefine %%p
				%endif
			%elif _pos > 6
				%deftok %%t %%ss
				push_token %%t, 0
			%else
				%assign %%m _pos
			%endif
		%elif %%m = 1
			%deftok %%t %%ss
			%xdefine %%p %[%%p]%[%%t]
			%if _pos = 5
				push_token [%[%%p], %%m
				%xdefine %%p
				%assign %%m -1
			%endif
		%elif %%m = 2
			%deftok %%t %%ss
			%xdefine %%p %[%%p]%[%%t]
			%if _pos = 5
				push_token %[%%p], %%m
				%xdefine %%p
				%assign %%m -1
			%endif
		%else
			%deftok %%t %%ss
			%xdefine %%p %[%%p]%[%%t]
			%if _pos = 6
				push_token %%p, %%m
				%xdefine %%p
				%assign %%m -1
			%endif
		%endif
		%assign %%i %%i + 1
	%endrep
	%ifnempty %%p
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
				push_reg
			%elifidn _rpn_%[%%n], -
				%warning vp_sub %%r1, %%r0
				push_reg
			%elifidn _rpn_%[%%n], *
				%warning vp_mul %%r1, %%r0
				push_reg
			%elifidn _rpn_%[%%n], &
				%warning vp_and %%r1, %%r0
				push_reg
			%elifidn _rpn_%[%%n], ^
				%warning vp_xor %%r1, %%r0
				push_reg
			%elifidn _rpn_%[%%n], |
				%warning vp_or %%r1, %%r0
				push_reg
			%elifidn _rpn_%[%%n], /
				push_reg
				push_reg
				get_reg _reg_sp
				%warning vp_xor _reg, _reg
				%warning vp_div %%r1, _reg, %%r0
				pop_reg
			%elifidn _rpn_%[%%n], %
				push_reg
				push_reg
				get_reg _reg_sp
				%warning vp_xor _reg, _reg
				%warning vp_div %%r1, _reg, %%r0
				%warning vp_cpy _reg, %%r0
				pop_reg
			%else
				%error Unknown token _rpn_%[%%n]
			%endif
		%elif _rpn_type_%[%%n] = 2
			;address of
			get_reg _reg_sp
			push_reg
			%warning vp_lea _rpn_%[%%n], _reg
		%elif _rpn_type_%[%%n] = 3
			;path bind
			get_reg _reg_sp
			push_reg
			%warning fn_bind _rpn_%[%%n], _reg
		%elif _rpn_type_%[%%n] = 4
			;relative label
			get_reg _reg_sp
			push_reg
			%warning vp_rel _rpn_%[%%n], _reg
		%else
			;symbol
			get_reg _reg_sp
			push_reg
			%warning vp_cpy _rpn_%[%%n], _reg
		%endif
		%assign %%n %%n + 1
	%endrep
%endmacro

def_local
	def_local_long	a
	def_local_long	b
	def_local_long	c
	def_local_long	d
	def_local_long	e
	def_local_long	f
def_local_end

set_reg_stack r2, r1, r10

set_token_list @path/to/test $label :.f
print_token_list
token_to_rpn
print_rpn_list
emit_rpn_list

set_token_list .a & .b | .d ^ .f + .e + .c
print_token_list
token_to_rpn
print_rpn_list
emit_rpn_list

set_token_list .a + (.b + .c) + .d + .e + .f
print_token_list
token_to_rpn
print_rpn_list
emit_rpn_list

get_reg_stack
