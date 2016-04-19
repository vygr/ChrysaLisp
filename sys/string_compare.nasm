%include 'inc/func.inc'

	fn_function sys/string_compare, no_debug_enter
		;inputs
		;r0 = string1
		;r1 = string2
		;outputs
		;r0 = 0 if not same
		;trashes
		;r0-r3

		vp_xor r2, r2
		vp_xor r3, r3
		loop_start
			vp_cpy_b [r0], r2
			vp_cpy_b [r1], r3
			breakif r2, !=, r3
			if r2, ==, 0
				vp_ret
			endif
			vp_inc r0
			vp_inc r1
		loop_end
		vp_xor r0, r0
		vp_ret

	fn_function_end
