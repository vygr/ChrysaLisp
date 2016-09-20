%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/task_open
		;inputs
		;r0 = name string object
		;outputs
		;r0, r1 = mailbox id
		;trashes
		;all but r4

		s_bind sys_task, statics, r1
		s_jmp sys_task, open_device, {r0, [r1 + tk_statics_cpu_id]}

	def_function_end
