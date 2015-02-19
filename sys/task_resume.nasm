%include "func.inc"
%include "task.inc"

	fn_function "sys/task_resume"
		;inputs
		;r15 = task control node
		;r0 = task control node (to resume)
		;trashes
		;outputs
		;r15 = task control node
		;trashes
		;r0-r2

		;save
		vp_cpy r0,r2

		;remove task control block from suspend list
		vp_cpy r0, r1
		ln_remove_node r0, r1

		;get statics
		fn_bind sys/task_statics, r0

		;add to task list
		vp_lea [r0 + TK_STATICS_TASK_LIST], r1
		lh_add_at_head r1, r2, r0
		vp_ret

	fn_function_end
