%include "func.inc"
%include "task.inc"

	fn_function "sys/task_suspend"
		;inputs
		;r15 = task control node

		;push task state
		tk_save_state

		;save stack pointer
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;remove task control block
		vp_cpy r15, r0
		ln_remove_node r0, r15

		;restore next task
		fn_jmp sys/task_restore

	fn_function_end
