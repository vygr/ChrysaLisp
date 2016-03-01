%include "inc/func.inc"
%include "inc/task.inc"

	fn_function "sys/task_restore"
		;restore next task
		;r15 = control block to restore

		;round robin past any list head
		ln_get_forward r15, r0
		fn_bind sys/task_statics, r0
		vp_cpy r15, [r0 + TK_STATICS_CURRENT_TCB]

		;restore old stack pointer
		vp_cpy [r15 + TK_NODE_STACK], r4

		;pop task state
		tk_load_state
		vp_ret

	fn_function_end
