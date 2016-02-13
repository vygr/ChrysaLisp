%include "func.inc"
%include "task.inc"

	fn_function "sys/task_restore"
		;restore next task

		ln_get_succ r15, r0
		if r0, ==, 0
			vp_cpy [r15 - LH_LIST_TAIL], r15
		endif

		;restore old stack pointer
		vp_cpy [r15 + TK_NODE_STACK], r4

		;pop task state
		tk_load_state
		vp_ret

	fn_function_end
