%include "func.inc"
%include "list.inc"

	fn_function "sys/task_resume"
		;inputs
		;r15 = task control node
		;r0 = task control node (to resume)
		;outputs
		;r15 = task control node
		;trashes
		;r0-r2

		;save
		vp_cpy r0,r2

		;remove task control block from suspend list
		vp_cpy r0, r1
		ln_remove_node r0, r1

		;add to task list
		ln_add_node_before r15, r2, r0
		vp_ret

	fn_function_end
