%include 'inc/func.inc'

	fn_function sys/string_from_long
		;inputs
		;r0 = number
		;r1 = string buffer
		;r2 = base
		;trashes
		;r0-r3, r5

		vp_cpy r2, r3	;base
		vp_cpy r1, r5	;buffer start
		loop_start
			vp_xor r2, r2
			vp_div r3, r2, r0
			vp_add '0', r2
			if r2, >, '9'
				vp_add 'A' - ':', r2
			endif
			vp_cpy_ub r2, [r5]
			vp_inc r5
		loop_until r0, ==, 0
		vp_cpy_ub r0, [r5]
		loop_start
			vp_dec r5
			breakif r5, ==, r1
			vp_cpy_ub [r1], r2
			vp_cpy_ub [r5], r3
			vp_cpy_ub r3, [r1]
			vp_cpy_ub r2, [r5]
			vp_inc r1
		loop_until r1, ==, r5
		vp_ret

	fn_function_end
