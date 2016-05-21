%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_deinit

		;free the task heap
		static_bind sys_task, statics, r0
		s_jmp sys_heap, deinit, {&[r0 + tk_statics_task_heap]}

	fn_function_end
