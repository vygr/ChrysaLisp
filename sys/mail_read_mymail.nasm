%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function sys/mail_read_mymail, no_debug_enter
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		class_bind task, statics, r0
		vp_cpy [r0 + tk_statics_current_tcb], r0
		vp_lea [r0 + tk_node_mailbox], r0
		fn_jmp sys/mail_read

	fn_function_end
