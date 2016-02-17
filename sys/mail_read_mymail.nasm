%include "func.inc"
%include "task.inc"

	fn_function "sys/mail_read_mymail"
		;outputs
		;r0 = mailbox address
		;r1 = mail address
		;trashes
		;r0-r2

		fn_bind sys/task_statics, r0
		vp_cpy [r0 + TK_STATICS_CURRENT_TCB], r0
		vp_lea [r0 + TK_NODE_MAILBOX], r0
		fn_call sys/mail_read
		vp_ret

	fn_function_end
