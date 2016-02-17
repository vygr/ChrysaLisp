%include "func.inc"
%include "task.inc"

	fn_function "sys/task_deshedule"
		;push task state
		tk_save_state

		;save old stack pointer
		fn_bind sys/task_statics, r15
		vp_cpy [r15 + TK_STATICS_CURRENT_TCB], r15
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;get next task control block
		ln_get_succ r15, r15
		fn_jmp sys/task_restore

	fn_function_end
