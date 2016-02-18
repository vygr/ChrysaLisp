%include "func.inc"
%include "task.inc"

	fn_function "sys/mail_read_mymail"
		;outputs
		;r0 = mailbox address
		;r1 = mail address
		;trashes
		;r2

		fn_bind sys/task_statics, r1
		vp_cpy [r1 + TK_STATICS_CURRENT_TCB], r1
		vp_lea [r1 + TK_NODE_MAILBOX], r0
		lh_is_empty r0, r2
		if r2, ==, 0
			vp_cpy r1, [r0 + ML_MAILBOX_TCB]
			fn_call sys/task_suspend
		endif
		lh_remove_head r0, r1, r2
		vp_ret

	fn_function_end
