%include "func.inc"
%include "task.inc"

	fn_function "sys/task_deshedule"
		;inputs
		;r15 = task control node

		;push all old task registers
		vp_push r0
		vp_push r1
		vp_push r2
		vp_push r3
		vp_push r5
		vp_push r6
		vp_push r7
		vp_push r8
		vp_push r9
		vp_push r10
		vp_push r11
		vp_push r12
		vp_push r13
		vp_push r14

		;save old stack pointer
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;get next task control block
		ln_get_succ r15, r15
		fn_jmp sys/task_restore

	fn_function_end
