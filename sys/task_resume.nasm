%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/task.inc'

	fn_function sys/task_resume, no_debug_enter
		;inputs
		;r0 = task control node (to resume)
		;trashes
		;r1-r2

		;are we in suspend state ?
		vp_cpy [r0 + tk_node_node], r1
		if r1, ==, 0
			;add to task list
			static_bind sys_task, statics, r1
			vp_cpy [r1 + tk_statics_current_tcb], r1
			ln_add_node_before r1, r0, r2
		endif
		vp_ret

	fn_function_end
