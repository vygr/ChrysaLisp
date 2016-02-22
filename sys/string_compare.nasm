%include "func.inc"

	fn_function "sys/string_compare"
		;inputs
		;r0 = string1
		;r1 = string2
		;outputs
		;r0 = 0 if not same
		;trashes
		;r0-r3

		loop_start
			vp_cpy byte[r0], r2l
			vp_cpy byte[r1], r3l
			vp_and 0xff, r2
			vp_and 0xff, r3
			breakif r2, !=, r3
			if r2, ==, 0
				vp_cpy 1, r0
				vp_ret
			endif
			vp_inc r0
			vp_inc r1
		loop_end
		vp_xor r0, r0
		vp_ret

	fn_function_end
