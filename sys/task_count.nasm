%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/task_count
		;outputs
		;r0 = task count

		s_bind sys_task, statics, r0
		vp_cpy [r0 + tk_statics_task_count], r0
		vp_ret

	def_function_end
