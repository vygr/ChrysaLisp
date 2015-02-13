%include "func.inc"
%include "task.inc"

	fn_function "sys/task_sleep"
		;inputs
		;r15 = task control node
		;r0 = time delay in usec
		;trashes
		;r0-r2

		;calculate wake time
		vp_sub TIMEVAL_SIZE, r4
		vp_cpy r0, r1
		vp_xor r0, r0
		vp_cpy r0, [r4 + TIMEVAL_SEC]
		vp_cpy r0, [r4 + TIMEVAL_USEC]
		vp_cpy r4, r0
		sys_gettimeofday r0, 0
		vp_cpy [r4 + TIMEVAL_SEC], r2
		vp_mul 1000000, r2
		vp_add [r4 + TIMEVAL_USEC], r2
		vp_add r1, r2
		vp_cpy r2, [r15 + TK_NODE_TIME]
		vp_add TIMEVAL_SIZE, r4

		;remove task control block
		vp_cpy r15, r0
		vp_cpy r15, r1
		ln_remove_node r0, r15

		;get statics
		fn_call sys/task_get_statics

		;add to timer list and restore next
		vp_lea [r0 + TK_STATICS_TASK_TIMER_LIST], r0
		lh_add_at_head r0, r1, r2
		fn_jmp sys/task_restore

	fn_function_end
