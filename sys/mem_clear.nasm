%include "func.inc"

	fn_function "sys/mem_clear"
		;inputs
		;r0 = address
		;r1 = length in bytes
		;trashes
		;r0-r2

		vp_add r0, r1
		vp_xor r2, r2
		loop_start
			vp_cpy r2l, byte[r0]
			vp_inc r0
		loop_until r0, ==, r1
		vp_ret

	fn_function_end
