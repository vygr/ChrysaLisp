%include "inc/func.inc"
%include "inc/list.inc"
%include "inc/task.inc"

	fn_function "sys/task_resume"
		;inputs
		;r0 = task control node (to resume)
		;trashes
		;r1-r2

		;add to task list
		fn_bind sys/task_statics, r1
		vp_cpy [r1 + TK_STATICS_CURRENT_TCB], r1
		ln_add_node_before r1, r0, r2
		vp_ret

	fn_function_end
