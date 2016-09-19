%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_stop
		;remove task control block
		s_bind sys_task, statics, r0
		vp_cpy [r0 + tk_statics_current_tcb], r1
		vp_cpy r1, r2
		ln_remove_node r2, r15

		;decrement task count
		vp_cpy [r0 + tk_statics_task_count], r2
		vp_dec r2
		vp_cpy r2, [r0 + tk_statics_task_count]

		;free our task control block
		vp_lea [r0 + tk_statics_task_heap], r0
		hp_freecell r0, r1, r2
		s_jmp sys_task, restore

	fn_function_end
