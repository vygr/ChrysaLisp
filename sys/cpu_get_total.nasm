%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/cpu_get_total
		;outputs
		;r0 = cpu total

		s_bind sys_task, statics, r0
		vp_cpy [r0 + tk_statics_cpu_total], r0
		vp_ret

	def_function_end
