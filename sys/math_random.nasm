%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/math_random
		;inputs
		;r0 = random range
		;outputs
		;r0 = random number in range
		;trashes
		;r1-r2

		vp_cpy r0, r1
		vp_cpy [rel seed], r0
		vp_mul 17, r0
		vp_cpy 0xa5a5a5a5a5a5a5a5, r2
		vp_xor r2, r0
		vp_cpy r0, [rel seed]
		vp_xor r2, r2
		vp_div r1
		vp_cpy r2, r0
		vp_ret

		align 8, db 0
	seed:
		dq 1234567890

	fn_function_end
