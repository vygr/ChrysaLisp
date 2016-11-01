%include 'inc/func.inc'
%include 'inc/task.inc'

def_func sys/task_init
	;set up current tcb
	f_bind sys_task, statics, r3
	vp_lea [r3 + tk_statics_task_list + lh_list_tail], r15
	vp_cpy r15, [r3 + tk_statics_current_tcb]

	;init task control block heap
	f_call sys_heap, init, {&[r3 + tk_statics_task_heap], (tk_node_size + ptr_size), ((tk_node_size + ptr_size) * 16)}

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

def_func_end
