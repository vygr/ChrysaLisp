%include "func.inc"
%include "task.inc"

	fn_function "sys/task_get_statics"
		;outputs
		;r0 = statics pointer

		vp_lea [rel statics], r0
		vp_ret

		align 8, db 0
	statics:
		times TK_STATICS_SIZE db 0

	fn_function_end
