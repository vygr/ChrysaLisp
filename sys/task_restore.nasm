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

		;pop all new registers
		vp_pop r14
		vp_pop r13
		vp_pop r12
		vp_pop r11
		vp_pop r10
		vp_pop r9
		vp_pop r8
		vp_pop r7
		vp_pop r6
		vp_pop r5
		vp_pop r3
		vp_pop r2
		vp_pop r1
		vp_pop r0
		vp_ret

	fn_function_end
