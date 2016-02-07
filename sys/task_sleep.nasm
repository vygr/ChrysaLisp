%include "func.inc"
%include "task.inc"

	fn_function "sys/task_sleep"
		;inputs
		;r15 = task control node
		;r0 = time delay in usec

		;push all task registers
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

		;save stack pointer
		vp_cpy r4, [r15 + TK_NODE_STACK]

		;save timeout
		vp_cpy r0, r1

		;calculate wake time
		fn_call sys/get_cpu_time
		vp_add r1, r0
		vp_cpy r0, [r15 + TK_NODE_TIME]

		;remove task control block
		vp_cpy r15, r2
		vp_cpy r15, r1
		ln_remove_node r2, r15

		;get statics
		fn_bind sys/task_statics, r3

		;add to timer list and restore next task
		vp_cpy [r3 + TK_STATICS_TASK_TIMER_LIST + LH_LIST_HEAD], r2
		repeat
			vp_cpy r2, r5
			ln_get_succ r2, r2
			breakif r2, ==, 0
		until r0, <, [r5 + TK_NODE_TIME]
		ln_add_node_before r5, r1, r0
		fn_jmp sys/task_restore

	fn_function_end
