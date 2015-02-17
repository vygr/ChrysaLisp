%include "func.inc"

	fn_function "sys/get_cpu_id"
		;outputs
		;r0 = cpu ID

		vp_cpy 0, r0
		vp_ret

	fn_function_end
