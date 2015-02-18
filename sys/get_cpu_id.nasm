%include "func.inc"

	fn_function "sys/get_cpu_id"
		;outputs
		;r0 = cpu ID

		vp_cpy [rel cpu_id], r0
		vp_ret

		align 8, db 0
	cpu_id:
		dq	0

	fn_function_end
