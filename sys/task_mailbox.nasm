%include 'inc/func.inc'
%include 'inc/task.inc'

	def_function sys/task_mailbox
		;outputs
		;r0, r1 = current task mailbox id

		s_bind sys_task, statics, r0
		vp_cpy [r0 + tk_statics_cpu_id], r1
		vp_cpy [r0 + tk_statics_current_tcb], r0
		vp_add tk_node_mailbox, r0
		vp_ret

	def_function_end
