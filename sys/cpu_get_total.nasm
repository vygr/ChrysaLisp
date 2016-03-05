%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/cpu_get_total, no_debug_enter
		;outputs
		;r0 = cpu total

		class_bind task, statics, r0
		vp_cpy [r0 + tk_statics_cpu_total], r0
		vp_ret

	fn_function_end
