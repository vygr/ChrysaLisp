%include "func.inc"
%include "task.inc"

	fn_function "sys/task_restore"
		;restore next task
		;r15 = control block

		;round robin past any list head
		ln_get_forward r15, r0

		;restore old stack pointer
		vp_cpy [r15 + TK_NODE_STACK], r4

		;pop task state
		tk_load_state
		vp_ret

	fn_function_end
