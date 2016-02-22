%include "func.inc"

	fn_function "sys/mem_copy"
		;inputs
		;r0 = source address
		;r1 = destination address
		;r2 = length in bytes
		;trashes
		;r0-r3

		vp_add r0, r2
		loop_start
			vp_cpy byte[r0], r3l
			vp_cpy r3l, byte[r1]
			vp_inc r0
			vp_inc r1
		loop_until r0, ==, r2
		vp_ret

	fn_function_end
