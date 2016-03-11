%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_tcb, no_debug_enter
		;outputs
		;r0 = current task tcb

		static_bind task, statics, r0
		vp_cpy [r0 + tk_statics_current_tcb], r0
		vp_ret

	fn_function_end
