%include "func.inc"
%include "task.inc"

	fn_function "sys/task_deshedule"
		;inputs
		;r15 = task control node

		;push task state
		tk_save_state

		;save old stack pointer
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;get next task control block
		ln_get_succ r15, r15
		fn_jmp sys/task_restore

	fn_function_end
