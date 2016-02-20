%include "func.inc"
%include "task.inc"

	fn_function "sys/mail_read_mymail"
		;outputs
		;r0 = mail address
		;trashes
		;r1-r2

		fn_bind sys/task_statics, r0
		vp_cpy [r0 + TK_STATICS_CURRENT_TCB], r0
		vp_lea [r0 + TK_NODE_MAILBOX], r1
		lh_is_empty r1, r2
		if r2, ==, 0
			vp_cpy r0, [r1 + ML_MAILBOX_TCB]
			fn_call sys/task_suspend
		endif
		lh_remove_head r1, r0, r2
		vp_ret

	fn_function_end
