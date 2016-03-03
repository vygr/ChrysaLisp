%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_deinit
		;get task statics
		fn_bind sys/task_statics, r0

		;free the task heap
		vp_lea [r0 + tk_statics_task_heap], r0
		fn_jmp sys/heap_deinit

	fn_function_end
