%include "func.inc"
%include "list.inc"

	fn_function "sys/task_resume"
		;inputs
		;r15 = task control node
		;r0 = task control node (to resume)
		;outputs
		;r15 = task control node
		;trashes
		;r0-r1

		;add to task list
		ln_add_node_before r15, r0, r1
		vp_ret

	fn_function_end
