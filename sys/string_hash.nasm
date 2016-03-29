%include 'inc/func.inc'

	fn_function sys/string_hash, no_debug_enter
		;inputs
		;r0 = string
		;outputs
		;r1 = string hash
		;trashes
		;r0, r2

		vp_cpy 5381, r1
		loop_start
			vp_xor r2, r2
			vp_cpy_b [r0], r2
			breakif r2, ==, 0
			vp_inc r0
			vp_mul 33, r1
			vp_add r2, r1
		loop_end
		vp_ret

	fn_function_end
