%include 'inc/func.inc'

	fn_function sys/string_copy
		;inputs
		;r0 = string
		;r1 = string copy
		;outputs
		;r0 = string end
		;r1 = string copy end
		;trashes
		;r2

		loop_start
			vp_cpy byte[r0], r2l
			vp_cpy r2l, byte[r1]
			vp_inc r0
			vp_inc r1
			vp_and 0xff, r2
		loop_until r2, ==, 0
		vp_ret

	fn_function_end
