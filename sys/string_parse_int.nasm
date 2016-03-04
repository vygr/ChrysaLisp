%include 'inc/func.inc'

	fn_function sys/string_parse_int, no_debug_enter
		;inputs
		;r0 = string
		;r1 = base
		;outputs
		;r0 = number
		;trashes
		;r2-r3

		vp_cpy r0, r2
		vp_xor r0, r0
		loop_start
			vp_cpy byte[r2], r3l
			vp_and 0xff, r3
			breakif r3, ==, 0
			if r3, >=, 'a'
				vp_sub 'a' - 'A', r3
			endif
			if r3, >=, 'A'
				vp_sub ('A' - '9') - 1, r3
			endif
			vp_sub '0', r3
			vp_mul r1, r0
			vp_add r3, r0
			vp_inc r2
		loop_end
		vp_ret

	fn_function_end
