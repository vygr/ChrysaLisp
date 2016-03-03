%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function "sys/task_init"
		;set up current tcb
		fn_bind sys/task_statics, r3
		vp_lea [r3 + TK_STATICS_TASK_LIST + LH_LIST_TAIL], r15
		vp_cpy r15, [r3 + TK_STATICS_CURRENT_TCB]

		;init task control block heap
		vp_lea [r3 + TK_STATICS_TASK_HEAP], r0
		vp_cpy TK_NODE_SIZE, r1
		vp_cpy TK_NODE_SIZE*8, r2
		fn_call sys/heap_init

		;init task lists
		vp_lea [r3 + TK_STATICS_TASK_LIST], r0
		lh_init r0, r1
		vp_lea [r3 + TK_STATICS_TIMER_LIST], r0
		lh_init r0, r1

		;init cpu count and id
		vp_cpy 1, qword[r3 + TK_STATICS_CPU_TOTAL]
		vp_cpy 0, qword[r3 + TK_STATICS_CPU_ID]
		vp_ret

	fn_function_end
