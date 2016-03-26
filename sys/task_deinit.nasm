%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_deinit, no_debug_enter
		;get task statics
		static_bind sys_task, statics, r0

		;free the task heap
		vp_lea [r0 + tk_statics_task_heap], r0
		static_jmp sys_heap, deinit

	fn_function_end
