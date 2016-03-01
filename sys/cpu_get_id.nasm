%include "inc/func.inc"
%include "inc/task.inc"

	fn_function "sys/cpu_get_id"
		;outputs
		;r0 = cpu ID

		fn_bind sys/task_statics, r0
		vp_cpy [r0 + TK_STATICS_CPU_ID], r0
		vp_ret

	fn_function_end
