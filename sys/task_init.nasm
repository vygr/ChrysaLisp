%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_init, no_debug_enter
		;set up current tcb
		static_bind sys_task, statics, r3
		vp_lea [r3 + tk_statics_task_list + lh_list_tail], r15
		vp_cpy r15, [r3 + tk_statics_current_tcb]

		;init task control block heap
		static_call sys_heap, init, {:[r3 + tk_statics_task_heap], tk_node_size, (tk_node_size * 16)}

		;init task lists
		vp_lea [r3 + tk_statics_task_list], r0
		lh_init r0, r1
		vp_lea [r3 + tk_statics_timer_list], r0
		lh_init r0, r1

		;init cpu count, task count and id
		vp_xor r1, r1
		vp_cpy_cl 1, [r3 + tk_statics_cpu_total]
		vp_cpy r1, [r3 + tk_statics_cpu_id]
		vp_cpy r1, [r3 + tk_statics_task_count]
		vp_ret

	fn_function_end
