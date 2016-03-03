%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_init
		;set up current tcb
		fn_bind sys/task_statics, r3
		vp_lea [r3 + tk_statics_task_list + lh_list_tail], r15
		vp_cpy r15, [r3 + tk_statics_current_tcb]

		;init task control block heap
		vp_lea [r3 + tk_statics_task_heap], r0
		vp_cpy tk_node_size, r1
		vp_cpy tk_node_size*8, r2
		fn_call sys/heap_init

		;init task lists
		vp_lea [r3 + tk_statics_task_list], r0
		lh_init r0, r1
		vp_lea [r3 + tk_statics_timer_list], r0
		lh_init r0, r1

		;init cpu count and id
		vp_cpy 1, qword[r3 + tk_statics_cpu_total]
		vp_cpy 0, qword[r3 + tk_statics_cpu_id]
		vp_ret

	fn_function_end
