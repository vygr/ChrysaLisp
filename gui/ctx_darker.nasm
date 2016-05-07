%include 'inc/func.inc'

	fn_function gui/ctx_darker
		;inputs
		;r1 = colour
		;inputs
		;r1 = darker colour
		;trashes
		;r2, r3

		vp_cpy r1, r2
		vp_cpy 0xff000000, r3
		vp_and r3, r1
		vp_and 0x00fefefe, r2
		vp_shr 1, r2
		vp_add r2, r1
		vp_ret

	fn_function_end
