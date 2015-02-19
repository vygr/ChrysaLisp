%include "func.inc"
%include "task.inc"

	fn_function "sys/task_stop"
		;inputs
		;r15 = task control node
		;trashes
		;r0-r15

		;remove task control block
		vp_cpy r15, r0
		vp_cpy r15, r1
		ln_remove_node r0, r15

		;get statics
		fn_bind sys/task_statics, r0

		;free our task control block
		vp_lea [r0 + TK_STATICS_TASK_HEAP], r0
		hp_free_cell r0, r1, r2
		fn_jmp sys/task_restore

	fn_function_end
