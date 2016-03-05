%include 'inc/func.inc'
%include 'inc/list.inc'
%include 'inc/task.inc'

	fn_function sys/task_resume, no_debug_enter
		;inputs
		;r0 = task control node (to resume)
		;trashes
		;r1-r2

		;add to task list
		class_bind task, statics, r1
		vp_cpy [r1 + tk_statics_current_tcb], r1
		ln_add_node_before r1, r0, r2
		vp_ret

	fn_function_end
