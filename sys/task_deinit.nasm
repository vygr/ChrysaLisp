%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/task_deinit

		;free the task heap
		s_bind sys_task, statics, r0
		s_jmp sys_heap, deinit, {&[r0 + tk_statics_task_heap]}

	def_function_end
