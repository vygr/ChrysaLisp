%include "func.inc"
%include "task.inc"

	fn_function "sys/task_deinit_tasker"
		;get task statics
		fn_bind sys/task_statics, r0

		;free the task heap
		vp_lea [r0 + TK_STATICS_TASK_HEAP], r0
		fn_jmp sys/heap_deinit

	fn_function_end
