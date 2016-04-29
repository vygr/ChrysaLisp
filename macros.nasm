%include 'inc/code.inc'

%macro set_reg_stack 0
	%xdefine _reg_0 r0
	%xdefine _reg_1 r1
	%xdefine _reg_2 r2
	%xdefine _reg_3 r3
	%xdefine _reg_4 r5
	%xdefine _reg_5 r6
	%xdefine _reg_6 r7
	%xdefine _reg_7 r8
	%xdefine _reg_8 r9
	%xdefine _reg_9 r10
	%xdefine _reg_10 r11
	%xdefine _reg_11 r12
	%xdefine _reg_12 r13
	%xdefine _reg_13 r14
	%xdefine _reg_14 r15
	%assign _reg_sp 0
%endmacro

%macro get_reg 0
	%xdefine _reg _reg_%[_reg_sp]
%endmacro

%macro push_reg 0
	%assign _reg_sp _reg_sp + 1
%endmacro

%macro pop_reg 0
	%assign _reg_sp _reg_sp - 1
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

%macro push_token 1
	%xdefine _token_%[_token_total] %1
	%assign _token_total _token_total + 1
%endmacro

%macro set_token_list 1
	%defstr %%s %1
	%strlen %%l %%s
	%assign _token_total 0
	%xdefine %%p
	%assign %%m 0
	%assign %%i 1
	%rep %%l
		%substr %%ss %%s %%i, 1
		sub_string %%ss, '][()+-*/%&^|'
		%if %%m = 0
			%if _pos > 2
				%ifnempty %%p
					push_token %%p
					%xdefine %%p
				%endif
				%deftok %%t %%ss
				push_token %%t
			%elif _pos = 0
				%deftok %%t %%ss
				%xdefine %%p %[%%p]%[%%t]
			%else
				%assign %%m 1
				%deftok %%t %%ss
				%xdefine %%p %[%%p]%[%%t]
			%endif
		%else
			%deftok %%t %%ss
			%xdefine %%p %[%%p]%[%%t]
			%if _pos = 1
				%assign %%m 0
			%endif
		%endif
		%assign %%i %%i + 1
	%endrep
	%ifnempty %%p
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

%macro push_rpn 1
	%xdefine _rpn_%[_rpn_total] %1
	%assign _rpn_total _rpn_total + 1
%endmacro

%macro token_to_rpn 0
	%assign _rpn_total 0
	%assign %%o 0
	%assign %%n 0
	%rep _token_total
		%iftoken _token_%[%%n]
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
				get_op_presidence _token_%[%%n]
				%assign %%t _op
				%rep %%o
					%assign %%o %%o - 1
					get_op_presidence _op_%[%%o]
					%if %%t > _op
						push_rpn _op_%[%%o]
					%else
						%assign %%o %%o + 1
						%exitrep
					%endif
				%endrep
				%xdefine _op_%[%%o] _token_%[%%n]
				%assign %%o %%o + 1
			%endif
		%else
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

%macro emit_rpn_list 0
	%assign %%n 0
	%rep _rpn_total
		%iftoken _rpn_%[%%n]
			pop_reg
			get_reg
			%xdefine %%r1 _reg
			pop_reg
			get_reg
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
				get_reg
				%warning vp_xor _reg, _reg
				%warning vp_div %%r1, _reg, %%r0
				pop_reg
			%elifidn _rpn_%[%%n], %
				push_reg
				push_reg
				get_reg
				%warning vp_xor _reg, _reg
				%warning vp_div %%r1, _reg, %%r0
				%warning vp_cpy _reg, %%r0
				pop_reg
			%else
				%error Unknown token _rpn_%[%%n]
			%endif
		%else
			get_reg
			push_reg
			%warning vp_cpy _rpn_%[%%n], _reg
		%endif
		%assign %%n %%n + 1
	%endrep
	pop_reg
	get_reg
	%warning result in _reg
%endmacro

def_local
	def_local_long	a
	def_local_long	b
	def_local_long	c
	def_local_long	d
	def_local_long	e
	def_local_long	f
def_local_end

set_reg_stack
set_token_list .a * .b + .d / (.c - .e % .f) * (.a^.d)
print_token_list
token_to_rpn
print_rpn_list
emit_rpn_list
