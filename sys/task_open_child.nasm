%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/task_open_child
		;inputs
		;r0 = name string object
		;outputs
		;r0, r1 = new task mailbox ID
		;trashes
		;all but r4

		s_bind sys_task, statics, r1
		s_jmp sys_task, open_remote, {r0, [r1 + tk_statics_cpu_id]}

	fn_function_end
