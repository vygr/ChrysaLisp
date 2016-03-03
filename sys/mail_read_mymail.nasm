%include 'inc/func.inc'
%include 'inc/task.inc'

	fn_function "sys/mail_read_mymail"
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		fn_bind sys/task_statics, r0
		vp_cpy [r0 + TK_STATICS_CURRENT_TCB], r0
		vp_lea [r0 + TK_NODE_MAILBOX], r0
		fn_jmp sys/mail_read

	fn_function_end
